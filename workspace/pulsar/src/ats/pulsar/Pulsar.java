/*
 * Pulsar-Sequencer written by Atsushi Oka 
 * Copyright 2018 Atsushi Oka
 *
 * This file is part of Pulsar-Sequencer. 
 * 
 * Pulsar-Sequencer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * Pulsar-Sequencer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with Pulsar-Sequencer.  If not, see <https://www.gnu.org/licenses/>.
 */

package ats.pulsar;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.function.Consumer;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import javax.swing.JComboBox;
import javax.swing.Timer;

import org.jaudiolibs.jnajack.JackException;

import ats.kawapad.KawaPad;
import ats.metro.Metro;
import ats.metro.MetroSequence;
import ats.metro.MetroTrack;
import ats.metro.MetroTrack.SyncType;
import ats.pulsar.lib.SchemeUtils;
import ats.pulsar.lib.secretary.Invokable;
import ats.pulsar.lib.secretary.InvokablyRunnable;
import ats.pulsar.lib.secretary.SecretaryMessage;
import ats.pulsar.lib.secretary.scheme.SchemeSecretary;
import ats.pulsar.lib.swing.MersenneTwisterFast;
import gnu.lists.IString;
import gnu.lists.LList;
import gnu.lists.Pair;
import gnu.mapping.Procedure;
import gnu.mapping.Procedure0;
import gnu.mapping.Procedure1;
import gnu.mapping.Procedure3;
import gnu.mapping.ProcedureN;
import gnu.mapping.Values;
import gnu.math.DFloNum;
import kawa.standard.Scheme;

/**
 * Pulsar is a MIDI sequencer program which is controlled by a powerful computer
 * language which is called Scheme. Pulsar is written in JAVA and uses JACK
 * Audio Connection Kit to receive and send MIDI messages; Pulsar runs on any
 * Linux boxes, hopefully OSX and Windows as well.
 * <p>
 * Pulsar dynamically composes music by writing programs by Scheme. Scheme is a
 * dialect of a computer language which is called "lisp". Pulsar could be used
 * to develop automatic DJ application or automatic backing track generators
 * which composes on the fly.
 * <h3>Workflow</h3>
 * <i>TODO</i>
 * <p>
 * <h3>Document Model</h3>
 * This application has a slightly different concept of open/close files
 * comparative to those general music players. When a user opens a scheme script
 * on Pulsar, Pulsar merely executes it without any clean ups nor
 * initializations. For example, if a user opens an empty scheme script while
 * Pulsar is playing music, nothing will be happened and keeps the former state.
 * <p>
 * In Pulsar, scripts should implicitly initialize the state of the sequencer if
 * it is necessary because Pulsar does not do it automatically.
 * <p>
 * Pulsar has two important parameters :
 * <p>
 * <ul>
 * <li>main-file
 * <li>main-invokable
 * </ul>
 * <p>
 * The <code>main-file</code> is a file path to the main file which Pulsar is
 * currently referring. Pulsar keeps checking the timestamp of the file and
 * trying to detect file modification. Whenever Pulsar detects any timestamp
 * update on the file, Pulsar automatically reads it and execute.
 * <p>
 * The <code>main-invokable</code> is the invokable which initializes the state
 * of the sequencer. This method is the place where Pulsar starts a new song.
 * <p>
 * If a script file sets <code>main-invokable</code>, this is effectively the
 * application <i>opens</i> a new-file in the sense of general applications. A
 * script file could also leave <code>main-invokable</code> untouched.
 * <p>
 * This behavior is designed to be useful in some scenarios. For example, when a
 * user repeatedly updates/modifies the script file repeatedly in order to check
 * how it sounds, this behavior might help the user. If Pulse initializes the
 * sequencer whenever it reads a file, the music must restart from the
 * beginning. Users usually prefer it to keep the music playing and modify the
 * music slightly to see what will happen.
 * <p>
 * This behavior could probably be used as implementing sub-modules.
 * <h3>Pulsar API<h3>
 * 
 * @author Atsushi Oka
 */
public final class Pulsar extends Metro {
	static final Logger LOGGER = Logger.getLogger(Pulsar.class.getName());

	static void logError(String msg, Throwable e) {
		LOGGER.log(Level.SEVERE, msg, e);
	}
	static void logInfo(String msg) {
		// LOGGER.log(Level.INFO, msg);
		System.err.println(msg);
	}
	static void logWarn(String msg) {
		LOGGER.log(Level.WARNING, msg);
	}

	long shutdownWait = 1024;

	public static void registerLocalSchemeInitializers( SchemeSecretary schemeSecretary, Pulsar pulsar ) {
		schemeSecretary.registerSchemeInitializer( pulsar, new SecretaryMessage.NoReturnNoThrow<Scheme>() {
			@Override
			public void execute0( Scheme scheme, Object[] args ) {
				pulsar.postMessage( new Runnable() {
					@Override
					public void run() {
						// 6. This initializes the thread of Metro's message-queue.
						// See ats.pulsar.lib.secretary.scheme.SchemeSecretary#specialInit()
						SchemeSecretary.initializeSchemeForCurrentThreadStatic( scheme );
					}
				});
			}
		});
		schemeSecretary.registerSchemeInitializer( pulsar, new SecretaryMessage.NoReturnNoThrow<Scheme>() {
			@Override
			public void execute0( Scheme scheme, Object[] args ) {
				pulsar.initScheme( scheme );
			}
		});
	}
	public static void invokeLocalSchemeInitializers( SchemeSecretary schemeSecretary, Pulsar pulsar ) {
		schemeSecretary.invokeSchemeInitializers( pulsar );
	}

	@Override
	protected void onCreateThread() {
		super.onCreateThread();
		registerLocalSchemeInitializers( this.schemeSecretary, this );
	}
	
	/**
	 * Creates an instance of Pulsar object without opening any specific scheme
	 * file. When a user creates an object by this constructor, the sequencer
	 * remains closed after the application boots up. The user must explicitly
	 * open a file to use the application.
	 * @throws IOException 
	 */
	public Pulsar( boolean windowInterface, boolean httpInterface ) throws IOException {
		this.schemeSecretary = new SchemeSecretary();
		this.schemeSecretary.setDirectMeeting( false );
		KawaPad.registerGlobalSchemeInitializer( schemeSecretary );
		this.schemeSecretary.newScheme();

		Pulsar.registerLocalSchemeInitializers( schemeSecretary, this );
		Pulsar.invokeLocalSchemeInitializers( schemeSecretary, this );
		
		if ( windowInterface )
			this.pulsarGui = new PulsarGui( this );
		else
			this.pulsarGui = null;
		
		if ( httpInterface )
			this.pulsarHttp = new PulsarHttp( this, 8192 );
		else
			this.pulsarHttp = null;
		
		this.enabledTimer = true;
	}
	

	transient boolean enabledTimer = false;

	/**
	 * The main method which starts up the application. The application opens a file
	 * which is specified in argument values. When more than one arguments were
	 * passed to the method, only the first argument is taken. 
	 * @throws IOException 
	 */
	public static void main(String[] args) throws IOException {
		Pulsar pulsar = new Pulsar( true, true );
		if ( args.length == 0 ) {
		} else {
			pulsar.openMainFile(null, new File( args[0] ));
		}
	}
	
	PulsarGui pulsarGui;
	PulsarHttp pulsarHttp;

	boolean isGuiAvailable() {
		return pulsarGui != null;
	}

	boolean isQuitting = false;
	public void quit() {
		if ( isQuitting )
			return;
		isQuitting = true;
		
		if ( isGuiAvailable() ) {
			this.pulsarGui.quit();
		}
		close();
		if ( timer != null )
			timer.stop();
		
		executeQuitHook();
	}
	
    final Collection<Runnable> quitHook = new LinkedList<>();
    public void addQuitHook( Runnable runnable ) {
    	synchronized ( this.quitHook ) {
    		this.quitHook.add( runnable );
    	}
    }
    public void executeQuitHook() {
    	synchronized ( this.quitHook ) {
    		for ( Runnable r : this.quitHook ) {
    			try {
    				r.run();
    			} catch ( Throwable e ) {
    				logError("", e);
    			}
    		}
    	}
    }

	
	private final SchemeSecretary schemeSecretary;
	public SchemeSecretary getSchemeSecretary() {
		return schemeSecretary;
	}
	
	private void newScheme() {
		this.enabledTimer = false; 
		this.getSchemeSecretary().newScheme();
		this.enabledTimer = true; 
	}

	public Invokable createInvokable( Procedure procedure ) {
//		return InvokableSchemeProcedure.createSecretariallyInvokable( schemeSecretary, procedure );
		return getSchemeSecretary().createSecretarillyInvokable( procedure );
	}
	public Invokable createInvokable2( Procedure procedure ) {
//		return InvokableSchemeProcedure.createSecretariallyInvokable( getSchemeSecretary(), procedure );
		return getSchemeSecretary().createSecretarillyInvokable( procedure );
	}
	public Runnable createRunnableAndInvocable( Procedure procedure, Object... args) {
		return new InvokablyRunnable( getSchemeSecretary().createSecretarillyInvokable( procedure ), args );
	}

//	public static InvokableSchemeProcedure createInvocable(
//			Procedure invokable) {
//		return new InvokableSchemeProcedure(syncObj, environment, language, invokable);
//	}

	
	MersenneTwisterFast random = new MersenneTwisterFast( new int[] { 
			(int) System.currentTimeMillis(),
			0x123, 0x234, 0x345, 0x456,
	});
	
	
	/**
	 * This field specifies the invokable to reset all of the states inside the
	 * sequencer and effectively this method starts a song. Whenever a user call
	 * {@link Pulsar#rewind()}, this invokable will be invoked.
	 */
	transient Invokable mainProcedure = null;

	/**
	 * Sets the main-invokable object.
	 * 
	 * @see Pulsar#mainProcedure 
	 */
	public void setMainProcedure( Invokable mainProcedure ) {
		this.mainProcedure = mainProcedure;
	}
	
	/**
	 * Returns the main-invokable object.
	 * 
	 * @see Pulsar#mainProcedure 
	 */
	public Invokable getMainProcedure() {
		return mainProcedure;
	}
	
    /**
	 * This application is designed to implement Variable-Song-Length. Basically the
	 * sequencer is always playing a specific region of a song repeatedly and
	 * remains in the region and only when the user send a "cue" to the sequencer,
	 * the sequencer goes to next region. When users call {@link Pulsar#cue() },
	 * this invokable is invoked. 
	 */
	transient Invokable cueProcedure = null;
	/**
	 * Sets the cue-invokable object.
	 * 
	 * @see Pulsar#cueProcedure 
	 */
	public void setCueProcedure( Invokable cueProcedure ) {
		this.cueProcedure = cueProcedure;
	}
	/**
	 * Returns the cue-invokable object.
	 * 
	 * @see Pulsar#cueProcedure 
	 */
	public Invokable getCueProcedure() {
		return cueProcedure;
	}

	
	/**
	 * Stores the path of the file which modified the related-file-list property.
	 * This path is used to resolve a relative path by
	 * {@link Pulsar#loadScheme(File) } method.
	 */
	transient File relatedFileParent=null;

	/**
	 * The "related-files" is a concept which could implement the "playlist"
	 * function in Pulsar and maybe this could be use on other purposes,too.
	 * 
	 * The related-files is a place to stores a list of scheme script files and the
	 * files are shown on the combobox on the main screen. Users can execute any
	 * scheme script on the list anytime.
	 */
	final List<File> relatedFiles = new ArrayList<>();

	/**
	 * @see #relatedFiles
	 */
	public void clearRelatedFiles() {
		relatedFiles.clear();
	}

	/**
	 * @see #relatedFiles
	 */
	public List<File> getRelatedFiles() {
		return Collections.unmodifiableList( relatedFiles );
	}

	/**
	 * @see #relatedFiles
	 */
	public void setRelatedFiles( Collection<File> fileList ) {
		this.relatedFiles.clear();
		this.relatedFiles.addAll( fileList );
		this.relatedFileParent = mainFile;

		if ( isGuiAvailable() ) { 
			if ( this.pulsarGui.cb_relatedFiles != null ) {
				this.pulsarGui.cb_relatedFiles.removeAllItems();
				for ( File f : this.relatedFiles ) {
					this.pulsarGui.cb_relatedFiles.addItem(f.getPath());
				}
			}
		}
	}
	
	/**
	 * This method is not used now. The developers should not use this method in
	 * order to keep consistency.
	 */
	public void addRelatedFile( File f) {
		this.relatedFiles.add( f );
		if ( this.isGuiAvailable() ) {
			if ( this.pulsarGui.cb_relatedFiles != null ) {
				this.pulsarGui.cb_relatedFiles.addItem( f.getPath() );
			}
		}
	}

	/**
	 * Reset the current state of the sequencer except the current opened script
	 * filename. This causes the application to reload the script file.
	 */
	public void reset() {
		try {
			this.enabledTimer = false;
			
			// XXX "initLanguage" ... is this supposed to be executed before others?
			newScheme();
			
			this.execCleanupHook();
			if ( isGuiAvailable() )
				this.pulsarGui.guiClear();
			this.close();
			this.lastModifiedOfMainFile = NOT_DEFINED;
		} finally {
			this.enabledTimer = true;
		}
	}
	
	
    /**
	 * {@link Pulsar#rewind()} method resets the state of the object and calls main
	 * invokable to back to the state of beginning of the project. This method:9
	 * effectively invoke the main invokable. See {@link Pulsar#mainProcedure}
	 */
    public void rewind() { 
    	logInfo( "===rewind" );
    	setPlaying(false);
    	clearTracks();
    	if ( mainProcedure != null )
    		mainProcedure.invoke();
    }

    /**
     * Invokes cue-invokable. See {@link Pulsar#cueProcedure } 
     */
    public void cue() {
    	logInfo( "===cue" );
    	if ( cueProcedure != null )
    		cueProcedure.invoke();
    }
    
    /**
     * This method clears main invokable and state.
     */
    public void clear() { 
    	logInfo( "===clear" );
    	this.setPlaying(false);
    	this.mainProcedure = null;
    	this.cueProcedure = null;
		clearTracks();
    }
    
    /**
     * This hook objects will be invoked whenever reset() method is called.
     */
    final Collection<Runnable> cleanupHook = new LinkedList<>();
    
    /**
     * Add a hook that will be invoked whenever reset() method is called.
     */
    public void addCleanupHook( Runnable runnable ) {
    	synchronized ( cleanupHook ) { 
    		cleanupHook.add( runnable );
    	}
    }
    public void execCleanupHook( ) {
    	synchronized ( cleanupHook ) {
    		for ( Iterator<Runnable> i =cleanupHook.iterator(); i.hasNext(); ) {
    			Runnable runnable = i.next();
    			try {
    				runnable.run();
    			} catch ( Throwable e ) {
    				logError("CLEANUP HOOK: IGNORED AN EXCEPTION", e);
    			}
    		}
    		cleanupHook.clear();
    	}
    }
    

	public File getConfigDir() {
		final File configDir = new File( System.getProperty("user.home"), ".pulsar" );
		if ( ! configDir.isDirectory() ) {
			if (! configDir.mkdir() ) {
				logInfo( "WARNING : Failed to create the config directory." );
			}
		}
		return configDir;
	}

	public File getConfigFile() {
		// Configuration Directory
		final File configFile = new File( getConfigDir(), "init.scm" );
		if ( ! configFile.isFile() ) {
			try {
				configFile.createNewFile();
			} catch (IOException e) {
				logInfo( "WARNING : Failed to create the main config file." );
				e.printStackTrace();
			}
		}
		return configFile;
	}

	/**
	 * This mechanism is currently not used. See {@link #readHistoryFile(JComboBox)}
	 * @return
	 */
	public File getHistoryFile() {
		// Configuration Directory
		final File historyFile = new File( getConfigDir(), "history.txt" );
		if ( ! historyFile.isFile() ) {
			try {
				historyFile.createNewFile();
			} catch (IOException e) {
				logError( "WARNING : Failed to create the history file.", e );
			}
		}
		return historyFile;
	}

    /**
     * Currently this method is not used. 
     * @param comboBox
     */
	public void readHistoryFile(JComboBox<String> comboBox) {
		comboBox.removeAllItems();

		BufferedReader in = null;
		try {
			in = new BufferedReader( new FileReader( getHistoryFile() ) );
			String s = null;
			for (;;) {
				s = in.readLine();
				if ( s == null ) break;
				// if ( new File(s).isFile() ) {
					comboBox.addItem( s );
				// }
			}
		} catch (FileNotFoundException e1) {
			logError( "" , e1 );
		} catch (IOException e1) {
			logError( "" , e1 );
		} finally {
			if ( in != null )
				try {
					in.close();
				} catch (IOException e1) {
					logError( "" , e1 );
				}
		}
	}
	private static final int NOT_DEFINED = -1;

	final File configFile = getConfigFile();
	long lastModifiedOfConfigFile=NOT_DEFINED;

	/**
	 * This stores the File object that point to the current opening scheme file.
	 * Pulsar's process of opening file is depend on watchdog timer. The watchdog
	 * timer frequently check the file that the {@link Pulsar#mainFile} points if
	 * its timestamp value is modified. When it detect timestamp modification, the
	 * watchdog timer invoke {@link Pulsar#loadScheme(File)} method to load the
	 * script.
	 */
	File mainFile=null;
	
	/**
	 * This path is used whenever a relative path is given to the
	 * {@link Pulsar#loadScheme(File)} method.
	 */
	File parentFile=null;

	/**
	 * This field stores the last value of timestamp of the {@link Pulsar#mainFile }
	 */
	long lastModifiedOfMainFile=NOT_DEFINED;
	
	/**
	 * @see #mainFile
	 * @param parentFile
	 * @param mainFile
	 */
	public void setMainFile( File parentFile, File mainFile ) {
		this.mainFile   = mainFile;
		this.parentFile = parentFile;
		this.lastModifiedOfMainFile = NOT_DEFINED;
	}
	public File getMainFile() {
		return mainFile;
	}
	
	public void openMainFile( File parentFile, File mainFile ) throws IOException {
		if ( ! mainFile.isFile() )
			throw new RuntimeException( "The specified file does not exist (" + mainFile.getPath() + ")" );
		
		// See the comment.
		setMainFile( null , mainFile );
		
		if ( isGuiAvailable() ) {
			pulsarGui.frame.openFile( mainFile );
		}
	}
	

	public static Runnable createTimer( Pulsar pulsar, long delay, long interval, Invokable invokable ) {
		java.util.Timer timer = new java.util.Timer( true );
		timer.scheduleAtFixedRate( new java.util.TimerTask() {
			@Override
			public void run() {
				Object result = invokable.invoke();
				if ( Boolean.FALSE.equals( result ) ) {
					timer.cancel();
				}
			}
		}, delay, interval );

		pulsar.addCleanupHook( new Runnable() {
			@Override
			public void run() {
				timer.cancel();
			}
		});
		
		return new Runnable() {
			@Override
			public void run() {
				timer.cancel();
			}
		};
	}

	private final class TrackProcedure extends Procedure0 {
		private final Pair pair;

		private TrackProcedure( Pair pair ) {
			this.pair = pair;
		}

		@Override
		public Object apply0() throws Throwable {
			return pair;
		}
	}
	interface TempoTapperTempoNotifier {
		void notifyTempo( double beatPerMinute );
	}
	class TempoTapper {
		final List<TempoTapperTempoNotifier> notifiers = new ArrayList<>();
		void registerNotifier( TempoTapperTempoNotifier notifier ) {
			notifiers.add( notifier );
		}
		
		long prev_time = 0;
		int BUF_SIZE = 3;
		long TIMEOUT = 1000L*1000L*1000L*2L;
		void reset() {
			for ( int i=0;i<t.length; i++ )
				t[i]=0;
			tidx=0;
		}
		long t[] = new long[BUF_SIZE];
		int tidx= 0;
		
		public void tap( ) {
			long current_time = System.nanoTime();
			if ( prev_time == 0 ) {
				prev_time = current_time;
				return;
			}
			if ( TIMEOUT < current_time - prev_time ) {
				prev_time = current_time;
				reset();
				return;
			}

			long current_diff = current_time - prev_time ;
			logInfo( "Elapsed Time : " + current_diff );

			tidx ++;
			if( tidx < t.length ) {
			} else {
				tidx = 0;
			}

			t[tidx] = current_diff;
			prev_time = current_time;

			boolean isFull = true;
			long sum = 0;
			{
				for ( int i=0; i<t.length; i++ ) {
					if ( 0 < t[i] ) {
						sum += t[i];
					} else {
						isFull = false;
						break;
					}
				}
			}

			if ( isFull ) 
				try {
					double avg = (double)sum / t.length;
					double onemin = 1000L*1000L*1000L*60L;
					double beatsPerMinute =  onemin / avg  ;
					
					double currentBeatsPerMinute = getBeatsPerMinute();
					beatsPerMinute = ( beatsPerMinute + currentBeatsPerMinute * 2 ) / 3;
					logInfo( String.format( "%.2f / %.2f = %.2f", onemin , avg , beatsPerMinute  ) );
					
					setBeatsPerMinute( beatsPerMinute );
					
				} catch (JackException e1) {
					logError("", e1);
				}
		}

		public void setBeatsPerMinute(double beatsPerMinute) throws JackException {
			Pulsar.this.setBeatsPerMinute( (long) beatsPerMinute );
			for ( TempoTapperTempoNotifier n : notifiers ) {
				n.notifyTempo( beatsPerMinute );
			}
		}
	}
	final TempoTapper tempoTapper = new TempoTapper();
	
	Timer timer =null;
	// a watchdog Timer
	{
		this.timer = new Timer(1000, new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if ( enabledTimer ) {
					lastModifiedOfConfigFile  = checkLastModified( configFile,  lastModifiedOfConfigFile , (file)->{}   );
					lastModifiedOfMainFile    = checkLastModified( mainFile,    lastModifiedOfMainFile,    (file)->{if ( isGuiAvailable() ) pulsarGui.updateFilename(file);} );
				}
			}

			long checkLastModified( File file, long lastModified, Consumer<File> updateProc ) {
				if ( file == null ) {
					updateProc.accept(null);
					return NOT_DEFINED; // lastModified;
				}
				
				long newLastModified = file.lastModified();
				if ( newLastModified != lastModified ) {
					logInfo( "Detected that the file was modified." );
					
					try {
//						// XXX THIS IS BOGUS
//						if ( getScheme() != null)
//							loadScheme( file );
						// (Mon, 22 Jul 2019 17:22:39 +0900) 
						// Therefore it is executed sequentially now,
						// this getScheme() !=null check may not be necessary anymore.
						
						loadScheme( file );
						updateProc.accept( file );
					} catch (FileNotFoundException e) {
						logError( "" , e );
					}
					
					
				}
				return newLastModified;
			}
		});
		timer.setInitialDelay(250);
		timer.start(); 
	}

	/**
	 * Loads and executes the specified scheme script file.
	 * 
	 * If the specified path is a relative path, {@link Pulsar#loadScheme(File) }
	 * try to resolve the path from the {@link Pulsar#parentFile}. If {@link Pulsar#parentFile}
	 * is not specified, {@link Pulsar#loadScheme(File) } throws an exception.
	 * 
	 * @param file
	 * @throws FileNotFoundException
	 */
	public void loadScheme( File file ) throws FileNotFoundException {
		getSchemeSecretary().executeSecretarially(
			new SecretaryMessage.NoReturn<Scheme,FileNotFoundException>() {
				@Override
				public void execute0(Scheme scheme, Object[] args ) throws FileNotFoundException {
					SchemeUtils.execSchemeFromFile( getMetroLock(),  scheme, parentFile, file );
				}
			});
	}

	interface ConnectProc {
		void apply( Pulsar pulsar, String from, String to ) throws JackException;
		ConnectProc CONNECT = new ConnectProc() {
			@Override
			public void apply(Pulsar pulsar, String from, String to) throws JackException {
				pulsar.connectPort(from, to);
			}
		};
		ConnectProc DISCONNECT = new ConnectProc() {
			@Override
			public void apply(Pulsar pulsar, String from, String to) throws JackException {
				pulsar.disconnectPort(from, to);
			}
		};
	}
	
	static void connectProc(Pulsar pulsar, Object[] args, ConnectProc proc ) {
		ArrayDeque<Object> deque = new ArrayDeque<>( Arrays.asList( args ) );
		while ( 0 < deque.size() ) {
			Object fromObj = deque.pop();
			Object toObj = deque.pop();
			if ( fromObj == null || toObj == null ) {
				break;
			}
			String from = fromObj != null ? SchemeUtils.toString( fromObj ) : null;
			String to   = toObj   != null ? SchemeUtils.toString( toObj   ) : null;
			try {
				proc.apply(pulsar, from, to );
			} catch ( JackException e  ) {
				logError( "" , e );
			}
		}
	}

	MetroTrack procTrack( boolean requestPut, Object[] args ) {
		if ( 2 <= args.length  ) {
			logInfo( "put-seq! : " + Arrays.asList(args).toString() );
			
			for ( int i=0; i<args.length; i++ ) {
				args[i] = SchemeUtils.schemeNullCheck(args[i]);
			}

			// Process the value on index 0 of arguments.
			String name;
			Collection<String> tags;
			if ( args[0] instanceof Pair ) {
				LinkedList<String> list = new LinkedList<>( SchemeUtils.symbolListToStringList( ((Pair)args[0]) ) );

				if ( 0 < list.size() ) {
					name = SchemeUtils.schemeNullCheck( list.remove(0) );
				} else {
					name = null;
				}

				tags = list;
			} else {
				name = args[0] == null ? null : SchemeUtils.symbolToString( args[0] );
				tags = null;
			}

			// Process the value on index 1 of arguments.
			Procedure procedure;
			if ( args[1] instanceof Procedure ) {
				procedure = (Procedure) args[1];
			} else if ( args[1] instanceof Pair ) {
				procedure = new TrackProcedure((Pair)args[1]);
			} else {
				throw new IllegalArgumentException( "unsupported type of the argument" );
			}

			// Process the value on index 2 of arguments.
			SyncType syncType       = 3<=args.length && args[2] !=null ? SyncType.toSyncType( SchemeUtils.toString( args[2] ) ) : SyncType.IMMEDIATE;

			// Process the value on index 3 of arguments.
			String syncTrackName    = 4<=args.length && args[3] !=null ? SchemeUtils.anyToString( SchemeUtils.schemeNullCheck( args[3] ) ) : null;
			
			// Process the value on index 4 of arguments.
			double offset           = 5<=args.length && args[4] !=null ? SchemeUtils.toDouble( args[4] ) : 0.0d;

			SchemeSequence sequence = new SchemeSequence( Pulsar.this.createInvokable( procedure ) );

			// synchronized block added at (Mon, 29 Jul 2019 13:36:52 +0900)
			synchronized ( getMetroLock() ) {
				// The method getTrack() now throws an exception when it is null.
				// (Mon, 29 Jul 2019 12:21:50 +0900)
				MetroTrack syncTrack = syncTrackName == null ? null : getTrack( syncTrackName );
				MetroTrack track = createTrack( name, tags, sequence, syncType, syncTrack, offset );
				if ( requestPut ) {
					putTrack( track );
				}
				return track;
			}
		} else {
			throw new RuntimeException( "Invalid parameter. usage : (put-seq! [name] [lambda] [syncType(immediate|parallel|serial)] [sync-parent-name] [offset] ) " );
		}
	}

	/**
	 * Initializes an environment of scheme engine and defines API for the scripts.
	 * 
	 * @param scheme
	 *            the scheme instance to initialize.
	 */
	public void initScheme( Scheme scheme ) {
		SchemeUtils.defineVar( scheme, "open?" , new ProcedureN( "open?" ) {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				return running;
			}
		});
		SchemeUtils.defineVar( scheme, "open!" , new Procedure1() {
			@Override
			public Object apply1(Object arg0) throws Throwable {
				open( SchemeUtils.toString( arg0 ) );
				return Invokable.NO_RESULT;
			}
		});
		SchemeUtils.defineVar( scheme, "close!" , new ProcedureN() {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				close();
				return Invokable.NO_RESULT;
			}
		});
		SchemeUtils.defineVar( scheme, "output!" , new ProcedureN() {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				for ( Object o : args ) {
					String name = SchemeUtils.toString( o );
					createOutputPort( name );
				}
				return Invokable.NO_RESULT;
			}
		});
		SchemeUtils.defineVar( scheme, "input!" , new ProcedureN() {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				for ( Object o : args ) {
					String name = SchemeUtils.toString( o );
					createInputPort( name );
				}
				return Invokable.NO_RESULT;
			}
		});
		SchemeUtils.defineVar( scheme, "connect!" , new ProcedureN() {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				connectProc(Pulsar.this, args, ConnectProc.CONNECT );
				return Invokable.NO_RESULT;
			}
		});
		SchemeUtils.defineVar( scheme, "disconnect!" , new ProcedureN() {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				connectProc(Pulsar.this, args, ConnectProc.DISCONNECT );
				return Invokable.NO_RESULT;
			}
		});

		SchemeUtils.defineVar( scheme, "get-all-input" , new ProcedureN() {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				return Pair.makeList( getInputPorts().stream().map( (v)->SchemeUtils.toSchemeString(v) )
					.collect( Collectors.toList() ) );
			}
		});
		SchemeUtils.defineVar( scheme, "get-all-output" , new ProcedureN() {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				return Pair.makeList( getOutputPorts().stream().map( (v)->SchemeUtils.toSchemeString(v) )
					.collect( Collectors.toList() ) );
			}
		});

		SchemeUtils.defineVar( scheme, "set-main!" , new ProcedureN() {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				logInfo("set-main");
				if ( args.length == 1 ) {
					Procedure procedure = (Procedure)args[0];
					setMainProcedure( Pulsar.this.createInvokable(procedure) );
				} else {
					throw new RuntimeException( "invalid argument length" );
				}
				return Invokable.NO_RESULT;
			}
		});
		SchemeUtils.defineVar( scheme, "get-main" , new Procedure0() {
			@Override
			public Object apply0() throws Throwable {
				return getMainProcedure();
			}
		});
		SchemeUtils.defineVar( scheme, "set-cue!" , new ProcedureN() {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				logInfo("set-cue");
				if ( args.length == 1 ) {
					Procedure procedure = (Procedure)args[0];
					setCueProcedure( Pulsar.this.createInvokable( procedure ) );
				} else {
					throw new RuntimeException( "invalid argument length" );
				}
				return Invokable.NO_RESULT;
			}
		});
		SchemeUtils.defineVar( scheme, "set-playing!" , new ProcedureN() {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				if ( args.length == 0 ) {
					togglePlaying();
				} else if ( args.length == 1 ) {
					setPlaying( (Boolean)args[0] );
				} else {
					throw new RuntimeException( "invalid argument length" );
				}
				return Invokable.NO_RESULT;
			}
		});
		SchemeUtils.defineVar( scheme, "playing?" , new ProcedureN() {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				return getPlaying();
			}
		});
		SchemeUtils.defineVar( scheme, "play!" , new ProcedureN() {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				setPlaying( true ); 
				return Invokable.NO_RESULT;
			}
		});
		SchemeUtils.defineVar( scheme, "stop!" , new ProcedureN() {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				setPlaying( false ); 
				return Invokable.NO_RESULT;
			}
		});
		SchemeUtils.defineVar( scheme, "quit!" , new ProcedureN() {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				Thread t = new Thread() {
					@Override
					public void run() {
						schemeSecretary.executeSecretarially( new SecretaryMessage.NoReturnNoThrow<Scheme>() {
							@Override
							public void execute0(Scheme resource, Object[] args) {
								try {
									Thread.sleep( shutdownWait );
								} catch (InterruptedException e) {
									logWarn( e.getMessage() );
								}
								quit(); 
							}
						}, Invokable.NOARG );
					}
				};
				t.start();
				
				return "Now Pulsar will shutdown in " + shutdownWait + " milliseconds...";
			}
		});
		SchemeUtils.defineVar( scheme, "tap!" , new ProcedureN() {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				tempoTapper.tap(); 
				return Invokable.NO_RESULT;
			}
		});
		SchemeUtils.defineVar( scheme, "set-tempo!" , new ProcedureN() {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				if ( 0 < args.length ) {
					double bpm = SchemeUtils.toDouble(args[0]);
					tempoTapper.setBeatsPerMinute( bpm );
				}

				return Invokable.NO_RESULT;
			}
		});
		SchemeUtils.defineVar( scheme, "set-related-files!" , new ProcedureN() {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				if ( args.length == 1  ) {
					Pair p = (Pair)args[0];
					Collection<File> files= SchemeUtils.<Object,File>convertList( p, (v)->new File( SchemeUtils.anyToString(v) ) );
					setRelatedFiles( files );
				} else {
					throw new RuntimeException( "invalid argument length" );
				}
				return Invokable.NO_RESULT;
			}
		});
		SchemeUtils.defineVar( scheme, "get-related-files" , new ProcedureN() {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				Pair p = new Pair();
				ArrayList<IString> ss = new ArrayList<>();
				for ( File f : relatedFiles ) {
					ss.add( IString.valueOf( f.getPath() ) );
				}
				p.addAll( ss );
				return p;
			}
		});
		SchemeUtils.defineVar( scheme, "cue!" , new Procedure0( "cue!" ) {
			@Override
			public Object apply0() throws Throwable {
				cue();
				return Invokable.NO_RESULT;
			}
		});
		
		/**
		 * This function only reset the current scheme environment.
		 * See {@link Pulsar#reset }
		 */
		Procedure0 resetScheme = new Procedure0( "reset!" ) {
			@Override
			public Object apply0() throws Throwable {
				reset();
				return Invokable.NO_RESULT;
			}
		};
		SchemeUtils.defineVar( scheme, "reset!" , resetScheme );
		SchemeUtils.defineVar( scheme, "rewind!" , new Procedure0("rewind!") {
			@Override
			public Object apply0() throws Throwable {
				rewind();
				return Invokable.NO_RESULT;
			}
		});
		SchemeUtils.defineVar( scheme, "clear!" , new Procedure0( "clear!" ) {
			@Override
			public Object apply0() throws Throwable {
				clear();
				return Invokable.NO_RESULT;
			}
		});

		ProcedureN addTrack = new ProcedureN( "add-track!" ) {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				putTrack( procTrack( true, args ) );
				return Invokable.NO_RESULT;
			}
		};
		SchemeUtils.defineVar( scheme, "add!" , addTrack);
		SchemeUtils.defineVar( scheme, "add-track!" , addTrack);

		
		ProcedureN addAllTrackProc = new ProcedureN( "add-all-tracks!" ) {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				synchronized ( getMetroLock() ) {
					if ( args.length < 1 ) {
						throw new IllegalArgumentException( "add-all takes one argument at least." );
					}
					List<MetroTrack> tracks = new ArrayList<>();
					for ( int i=1; i<args.length; i++ ) {
						tracks.add( procTrack( false, (Object[]) args[i] ) );
					}
					putAllTracks( tracks );
				}
				return Invokable.NO_RESULT;
			}
		};
		SchemeUtils.defineVar( scheme, "add-all!" ,        addAllTrackProc );
		SchemeUtils.defineVar( scheme, "add-all-tracks!" , addAllTrackProc );
//		SchemeUtils.defineVar( scheme, "add-tracks!" , addAllTrack);

		ProcedureN deleteTrackProc = new ProcedureN() {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				List<MetroTrack> tracks= new ArrayList<>();
				synchronized ( getMetroLock() ) {
					for ( Object arg : args ) {
						if ( arg instanceof MetroTrack ) {
							tracks.add( (MetroTrack)arg );
						} else if ( arg instanceof IString ) {
							tracks.add( searchTrack( SchemeUtils.toString( arg )));
						} else {
							throw new IllegalArgumentException( "" );
						}
					}
					removeAllTracksGracefully( tracks, null);
					return Invokable.NO_RESULT;
				}
			}
		};

		ProcedureN deleteTrackProc0 = new ProcedureN() {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				if ( args.length == 1 ) {
					args[0] = SchemeUtils.schemeNullCheck( args[0] );
					String name = args[0] != null ? SchemeUtils.toString( args[0] ) : null;
					boolean graceful = 1 < args.length ? SchemeUtils.toBoolean( args[1] ) : false;   
					
					// synchronized block added at (Mon, 29 Jul 2019 13:36:52 +0900)
					synchronized ( getMetroLock() ) { 
						// getTrack() throws an exception when it is null. (Mon, 29 Jul 2019 12:21:50 +0900)
						MetroTrack track = searchTrack( name );
						
						// If track is not found, ignore it. 
						// It is not necessary to report it in this case.
						// (Mon, 29 Jul 2019 14:13:27 +0900)
						if ( track != null )
							track.removeGracefully( null );
						
//						if ( track != null )
//							removeTrack( track );
						
						return Invokable.NO_RESULT;
					}
				} else if ( 1 < args.length ) {
					boolean graceful = SchemeUtils.toBoolean( args[0] );   

					List<String> trackNameList= new ArrayList<>();
					for ( int i=1; i<args.length; i++ ) {
						args[i] = SchemeUtils.schemeNullCheck( args[i] );
						String name = args[i] != null ? SchemeUtils.toString( args[i] ) : null;
						if ( name != null ) {
							trackNameList.add( name );
						}
					}

					List<MetroTrack> tracks= new ArrayList<>();
					synchronized ( getMetroLock() ) {
						for ( String name : trackNameList )
							tracks.add( searchTrack( name ) );
						removeAllTracksGracefully( tracks, null  );
						return Invokable.NO_RESULT;
					}
				} else {
					throw new RuntimeException( "Invalid parameter. usage : (new-sequence [name] [lambda] ) " );
				}
			}
		};
		SchemeUtils.defineVar( scheme, "del!" ,       deleteTrackProc );
		SchemeUtils.defineVar( scheme, "del-track!" , deleteTrackProc );
		
		
		ProcedureN endTrackProc = new ProcedureN() {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				if ( args.length == 0  )
					return Invokable.NO_RESULT;
				if ( args.length == 1 )
					return Invokable.NO_RESULT;

				ArrayDeque queue = new ArrayDeque( Arrays.asList( args ) );
				Procedure onEnd = (Procedure) queue.removeLast();

				List<MetroTrack> tracks= new ArrayList<>();
				synchronized ( getMetroLock() ) {
					for ( Object arg : args ) {
						if ( arg instanceof MetroTrack ) {
							tracks.add( (MetroTrack)arg );
						} else if ( arg instanceof IString ) {
							tracks.add( searchTrack( SchemeUtils.toString( arg )));
						} else {
							throw new IllegalArgumentException( "" );
						}
					}
					removeAllTracksGracefully( tracks, new Runnable() {
						@Override
						public void run() {
							try {
								onEnd.apply0();
							} catch (Throwable e) {
								logError("", e);
							}
						}
					});
					return Invokable.NO_RESULT;
				}
			}
		};

		SchemeUtils.defineVar( scheme, "end!" , endTrackProc);
		SchemeUtils.defineVar( scheme, "end-track!" , endTrackProc);
		
		SchemeUtils.defineVar( scheme, "ls" , new Procedure0() {
			@Override
			public Object apply0() throws Throwable {
				synchronized ( getMetroLock() ) {
					ArrayList<String> list = new ArrayList<>( tracks.size() );
					for ( MetroTrack track :  tracks ) {
						MetroSequence sequence = track.getSequence();
						if ( sequence instanceof SchemeSequence ) {
							list.add( ((SchemeSequence)sequence).getTrackName() );
						}
					}
					return Pair.makeList(list);
				}

			}
		});
		Procedure0 clr = new Procedure0("clear") {
			@Override
			public Object apply0() throws Throwable {
				clearTracks();
				return Invokable.NO_RESULT;
			}
		};
		SchemeUtils.defineVar( scheme, "clear-tracks!" , clr);
		
		Procedure1 has = new Procedure1( "has?" ) {
			@Override
			public Object apply1(Object arg0) throws Throwable {
				String value = SchemeUtils.toString( SchemeUtils.schemeNullCheck( arg0 ) );
				if ( value == null ) {
					throw new IllegalArgumentException( "cannot be null" );
				}
				return searchTrack( value ) != null;
			}
		};
		SchemeUtils.defineVar( scheme, "has-seq?" , has );
		SchemeUtils.defineVar( scheme, "has?" , has );
		SchemeUtils.defineVar( scheme, "print-stack-trace" , new ProcedureN() {
			@Override
			public Object apply1(Object arg) throws Throwable {
				((Throwable)arg).printStackTrace();
				return arg;
			}
		});
		SchemeUtils.defineVar( scheme, "display-warn" , new ProcedureN() {
			@Override
			public Object apply1(Object arg) throws Throwable {
				System.err.print( arg );
				return Values.empty;
			}
		});
		SchemeUtils.defineVar( scheme, "newline-warn" , new Procedure0() {
			@Override
			public Object apply0() throws Throwable {
				System.err.println();
				return Values.empty;
			}
		});

		SchemeUtils.defineVar( scheme, "list-seq" , new ProcedureN() {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				//					logInfo("list-seq");
				synchronized ( getMetroLock() ) {
					ArrayList<LList> list = new ArrayList<>( tracks.size() );
					for ( MetroTrack track :  tracks ) {
						SchemeSequence sequence = (SchemeSequence)track.getSequence();
						list.add( SchemeUtils.acons( sequence.getTrackName(), sequence.asociationList ));
					}
					return LList.makeList(list);
				}
			}
		});

		SchemeUtils.defineVar( scheme, "typeof" , new ProcedureN() {
			public Object applyN(Object[] args) throws Throwable {
				if ( 0 < args.length  ) {
					return args[0].getClass().getName();
				} else {
					return Invokable.NO_RESULT;
				}
			}
		});

		SchemeUtils.defineVar( scheme, "mktimer" , new Procedure3() {
			@Override
			public Object apply3(Object arg0, Object arg1,Object arg2 ) throws Throwable {
				Runnable runnable = createTimer( Pulsar.this, 
					SchemeUtils.toInteger( arg0 ), 
					SchemeUtils.toInteger( arg1 ), 
					Pulsar.this.createInvokable2( (Procedure)arg2 ) );

				return new Procedure0() {
					public Object apply0() throws Throwable {
						runnable.run();
						return Values.noArgs;
					};
				};
			}
		});

		SchemeUtils.defineVar( scheme, "rnd" , new ProcedureN() {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				switch ( args.length ) {
					case 0 :
						return DFloNum.valueOf( random.nextDouble() );
					case 1 : {
						double range = SchemeUtils.toDouble( args[0] );
						return DFloNum.valueOf( random.nextDouble() * range );
					}
					default :
					{
						double rangeMin = SchemeUtils.toDouble( args[0] );
						double rangeMax = SchemeUtils.toDouble( args[1] );
						double range    = rangeMax - rangeMin;

						return DFloNum.valueOf( random.nextDouble() * range + rangeMin );
					}
				}
			}
		});
		
		SchemeUtils.defineVar( scheme, "luck" , new ProcedureN() {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				double probability = args.length == 0 ? 0.5 : SchemeUtils.toDouble( args[0] );
				if ( probability < 0 ) return false;
				if ( 1.0<=probability  ) return true;
				return random.nextBoolean( probability );
			}
		});

		{
			SchemeUtils.execScheme( Pulsar.class, scheme, "lib/init0.scm"  );
			SchemeUtils.execScheme( Pulsar.class, scheme, "lib/init.scm"  );
			SchemeUtils.execScheme( Pulsar.class, scheme, "lib/xnoop.scm" );
			//	    		execScheme( scheme, "event-parser.scm" );
		}
	}	
}
	
