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

package pulsar;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.lang.invoke.MethodHandles;
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

import gnu.expr.Keyword;
import gnu.lists.EmptyList;
import gnu.lists.IString;
import gnu.lists.LList;
import gnu.lists.Pair;
import gnu.mapping.Procedure;
import gnu.mapping.Procedure0;
import gnu.mapping.Procedure1;
import gnu.mapping.Procedure3;
import gnu.mapping.ProcedureN;
import gnu.mapping.Symbol;
import gnu.mapping.Values;
import gnu.math.DFloNum;
import kawa.standard.Scheme;
import kawapad.KawaPad;
import metro.Metro;
import metro.MetroTrack;
import metro.MetroTrack.SyncType;
import pulsar.lib.PulsarLogger;
import pulsar.lib.scheme.SchemeUtils;
import pulsar.lib.scheme.scretary.SchemeSecretary;
import pulsar.lib.secretary.Invokable;
import pulsar.lib.secretary.InvokablyRunnable;
import pulsar.lib.secretary.SecretaryMessage;
import pulsar.lib.swing.MersenneTwisterFast;

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
	static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
	static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE,   msg, e   ); }
	static void logInfo (String msg             ) { LOGGER.log(Level.INFO,     msg      ); }
	static void logWarn (String msg             ) { LOGGER.log(Level.WARNING,  msg      ); }

	static String messageWarnIgnoredMissingSyncTrack( Object arg ) {
		return "could not find a track which name was " + arg + " ... ignored.";
	}
	
	long shutdownWait = 1024;

	public static void registerLocalSchemeInitializers( SchemeSecretary schemeSecretary, Pulsar pulsar ) {
		schemeSecretary.registerSchemeInitializer( pulsar, new SecretaryMessage.NoReturnNoThrow<Scheme>() {
			@Override
			public void execute0( Scheme scheme, Object[] args ) {
				/*
				 * INIT_02
				 * We give up to initialize current thread if it is not running.
				 * This might happen in initializing the object in the constructor method.
				 * This seems to be a problem, but it will be initialized when Metro is opened
				 * by open() method; therefore, we temporally give up the initializing process.  
				 */
				if ( pulsar.isOpened() ) {
					logWarn( "registerLocalSchemeInitializers_POSTMESSAGE  DONE" );
					pulsar.postMessage( new Runnable() {
						@Override
						public void run() {
							// 6. This initializes the thread of Metro's message-queue.
							// See pulsar.lib.secretary.scheme.SchemeSecretary#specialInit()
							SchemeSecretary.initializeSchemeForCurrentThreadStatic( scheme );
						}
					});
				} else {
					logWarn( "registerLocalSchemeInitializers_POSTMESSAGE IGNORED" );
				}
			}
		});
		schemeSecretary.registerSchemeInitializer( pulsar, new SecretaryMessage.NoReturnNoThrow<Scheme>() {
			@Override
			public void execute0( Scheme scheme, Object[] args ) {
				pulsar.initScheme( scheme );
			}
		});
		schemeSecretary.registerSchemeInitializer( pulsar, new SecretaryMessage.NoReturnNoThrow<Scheme>() {
			@Override
			public void execute0( Scheme scheme, Object[] args ) {
				PulsarGuiUtils.initStaticScheme( pulsar, scheme );
			}
		});

	}
	public static void invokeLocalSchemeInitializers( SchemeSecretary schemeSecretary, Pulsar pulsar ) {
		schemeSecretary.invokeSchemeInitializers( pulsar );
	}

	@Override
	protected void onCreateThread() {
		super.onCreateThread();
//		registerLocalSchemeInitializers( this.schemeSecretary, this );
	}
	
	/**
	 * Creates an instance of Pulsar object without opening any specific scheme
	 * file. When a user creates an object by this constructor, the sequencer
	 * remains closed after the application boots up. The user must explicitly
	 * open a file to use the application.
	 * @throws IOException 
	 */
	public Pulsar( boolean windowInterface, boolean httpInterface, int httpPort ) throws IOException {
		super();
		
//		>>> VERSION 1 
//		this.schemeSecretary = new SchemeSecretary();
//		this.schemeSecretary.setDirectMeeting( false );
//		KawaPad.registerGlobalSchemeInitializer( schemeSecretary );
//		this.schemeSecretary.newScheme();
//
//		Pulsar.registerLocalSchemeInitializers( schemeSecretary, this );
//		Pulsar.invokeLocalSchemeInitializers( schemeSecretary, this );
//		<<< VERSION 1

		/*
		 * Search INIT_02 inside the entire workspace to know the modification of the
		 * order of Pulsar's initialization.
		 */
//		>>> VERSION INIT_02 (Sat, 03 Aug 2019 15:47:41 +0900)
		this.schemeSecretary = new SchemeSecretary();
		this.schemeSecretary.setDirectMeeting( true );
		KawaPad.registerGlobalSchemeInitializer( schemeSecretary );
		Pulsar.registerLocalSchemeInitializers( schemeSecretary, this );
//		<<< VERSION INIT_02 (Sat, 03 Aug 2019 15:47:41 +0900)
		
		if ( windowInterface )
			this.pulsarGui = new PulsarGui( this );
		else
			this.pulsarGui = null;
		
		if ( httpInterface )
			this.pulsarHttp = new PulsarHttp( this, httpPort );
		else
			this.pulsarHttp = null;
		
//		REMOVED >>> INIT_02 (Sat, 03 Aug 2019 15:47:41 +0900)
//		setting enableTime should be done only in newScheme(); 
//		this.enabledTimer = true;
//		REMOVED <<< INIT_02 (Sat, 03 Aug 2019 15:47:41 +0900)
		
		this.newScheme();
		
		// INIT_03 : it appears that INIT_02 which is a initial correction of
		// the initializing order of pulsar/kawapad is not sufficient.
		// Initializing scheme objects and initializing frames should be separately
		// initialized.
		// 
		// The method init() is called whenever the frame is created.
		if ( this.pulsarGui != null )
			this.pulsarGui.init();
	}

	/**
	 * The main method which starts up the application. The application opens a file
	 * which is specified in argument values. When more than one arguments were
	 * passed to the method, only the first argument is taken. 
	 * @throws IOException 
	 */
	static Pulsar parseArgsAndCreatePulsar( String[] args ) throws IOException {
		
		boolean argHttp = true;
		int     argHttpPort = 8192;
		boolean argGui = true;
		String  argFileName = null;
		
		for ( int i=0; i<args.length; i++ ) {
			String s = args[i];

			if ( s.startsWith( "--http=" ) ) {
				argHttp = true;
				argHttpPort = Integer.parseInt( s.substring( "--http=".length() ) );
				break;
			} else if ( s.equals( "--http" ) ) { 
				argHttp = true;
				argHttpPort = 8192;
				break;
			} else if ( s.equals( "--gui" ) ) { 
				argGui = true;
				break;
			} else if ( s.equals( "--no-http" ) ) { 
				argHttp = false;
				break;
			} else if ( s.equals( "--no-gui" ) ) { 
				argGui = false;
				break;
			} else {
				if ( argFileName == null )
					argFileName = s;
			}
		}

		if ( argHttp || argGui ) {
			Pulsar pulsar = new Pulsar( argGui , argHttp, argHttpPort );
			if ( argFileName != null )
				pulsar.openMainFile(null, new File( argFileName ));
			
			return pulsar;
		} else {
			System.err.println( "hmm... you have to have at least one interface to control the system." );
			return null;
		}
	}

	transient boolean enabledTimer = false;

	public static void main(String[] args) throws IOException {
		PulsarLogger.init();
		parseArgsAndCreatePulsar(args);
	}
	
//	@Override
//	public void open(String clientName) throws JackException {
//		super.open(clientName);
//		newScheme();
//	}
	
	PulsarGui pulsarGui;
	PulsarHttp pulsarHttp;

	boolean isGuiAvailable() {
		return pulsarGui != null;
	}

	boolean isQuitting = false;
	
	/**
	 * Notify every interface to shutdown the application. Then shutdown. 
	 */
	public void quit() {
//		if ( isQuitting )
//			return;

		isQuitting = true;
		
		if ( isGuiAvailable() ) {
			this.pulsarGui.quit();
		} else {
			shutdown();
		}
		
//		moved to the close hook facility (Thu, 08 Aug 2019 00:46:56 +0900)
//		if ( timer != null )
//			timer.stop();
	}

    @Override
    public void close() {
    	super.close();
    }
    
    public void shutdown() {
		close();
    	executeShutdownHook();
    }

    final Collection<Runnable> shutdownHook = new LinkedList<>();
    public void addShutdownHook( Runnable runnable ) {
    	synchronized ( this.shutdownHook ) {
    		this.shutdownHook.add( runnable );
    	}
    }
    public void executeShutdownHook() {
    	synchronized ( this.shutdownHook ) {
    		for ( Runnable r : this.shutdownHook ) {
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
		logInfo("Pulsar#newScheme() "); 
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
		logInfo("===Pulsar.reset()");
		try {
			this.enabledTimer = false;
			
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
     * This is no longer used. (Mon, 05 Aug 2019 14:47:22 +0900) 
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
//    	>>> DELETED (Mon, 05 Aug 2019 14:44:30 +0900)
//    	this.mainProcedure = null;
//    	this.cueProcedure = null;
//    	<<<
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
		getSchemeSecretary().executeWithoutSecretarially( new SecretaryMessage.NoReturn<Scheme,IOException>() {
			@Override
			public void execute0(Scheme resource, Object[] args) throws IOException{
				logInfo( "Pulsar#openMainFile()" );
				if ( ! mainFile.isFile() )
					throw new RuntimeException( "The specified file does not exist (" + mainFile.getPath() + ")" );
				
				if ( isGuiAvailable() ) {
					pulsarGui.frame.openFile( mainFile );
				} else {
					// See the comment in the setMainFile().
					setMainFile( null , mainFile );
				}
			}
		}, Invokable.NOARG );
	}
	

	public static Runnable createTimer( Pulsar pulsar, long delay, long interval, Invokable invokable ) {
		java.util.Timer timer = new java.util.Timer( true );
		timer.scheduleAtFixedRate( new java.util.TimerTask() {
			@Override
			public void run() {
				// The life cycle of timer threads may not be able to be controlled by users;
				// therefore, we decided to initialize the environment every time we execute
				// timer events. (Mon, 05 Aug 2019 00:38:14 +0900)
				pulsar.getSchemeSecretary().initializeSchemeForCurrentThread();
				
				// Execute the specified process.
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

	private static final class TagSearchUserProcedure extends Procedure1 {
		private final Procedure proc;
		private final Object p;
		TagSearchUserProcedure(Object p, Procedure proc) {
			this.p = p;
			this.proc = proc;
		}
		@Override
		public Object apply1(Object arg1) throws Throwable {
			return proc.apply2( arg1, p );
		}
	}
	private static final class TagSearchAndProcedure extends Procedure1 {
		private final Pair p;
		TagSearchAndProcedure(Pair p) {
			this.p = p;
		}
		@Override
		public Object apply1(Object arg1) throws Throwable {
			Collection p2 = (Collection)arg1;
			return p2.containsAll( p );
		}
	}
	private static final class TagSearchOrProcedure extends Procedure1 {
		private final Pair p;
		TagSearchOrProcedure(Pair p) {
			this.p = p;
		}
		@Override
		public Object apply1(Object arg1) throws Throwable {
			Collection p2 = (Collection)arg1;
			for ( Object o : p ) {
				if ( p2.contains(o) ) 
					return true;
			}
			return false;
		}
	}
	private static final class TagSearchIsProcedure extends Procedure1 {
		private final Object value;
		TagSearchIsProcedure(Object value) {
			this.value = value;
		}
		@Override
		public Object apply1(Object arg1) throws Throwable {
			return value.equals( arg1 );
		}
	}

	final static class TrackProcedure extends Procedure0 {
		final Pair pair;
		TrackProcedure( Pair pair ) {
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
		if ( false ) {
			this.timer = new Timer(1000, new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {

					if ( false && enabledTimer ) {
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

			addShutdownHook( ()->{if ( timer != null ) timer.stop();} );
		}
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

	public MetroTrack createTrack( Object name, Collection<Object> tags, Procedure procedure ) {
		return createTrack( name, tags, new SchemeSequence( createInvokable( procedure ) ) );
	}

	/**
	 *  
	 * @param object
	 * @return
	 *    a newly created list which can safely be modified.
	 */
	static List<Object> readParamTrackName( Object object ) { 
		object = SchemeUtils.schemeNullCheck( object );
		
		if ( object instanceof Pair ) {
			return new ArrayList<>( (Pair)object );
		} else {
			return new ArrayList<>( Arrays.asList( SchemeUtils.schemeNullCheck( object ) ) );
		}
	}
	
	/**
	 * XXX 
	 * 
	 * @param object
	 * @return
	 */
	static Procedure readParamTrackSearcher( Object object ) { 
		object = SchemeUtils.schemeNullCheck( object );
		
		// TAG SEARCH
		if ( object instanceof Procedure ) {
			return (Procedure) object;
		} else if ( object instanceof Symbol ) {
			return new TagSearchIsProcedure((Symbol) object );
		} else if ( object instanceof Pair ) {
			// TODO DOCUMENT THIS
			Object car = ((Pair)object).getCar();
			Object cdr = ((Pair)object).getCar();
			if ( car instanceof Procedure ) {
				return new TagSearchUserProcedure(cdr, (Procedure) car ); 
			} else if ( car instanceof Keyword ) {
				switch ( ((Keyword)car).getName() ) {
					case "and" :
						return new TagSearchAndProcedure((Pair) object );
					case "or" :
						return new TagSearchOrProcedure((Pair) object );
					default : 
						throw new IllegalArgumentException( "The keyword " + car + " is not allowed here." );
				}
			} else {
				return new TagSearchAndProcedure((Pair) object );
			}
		} else {
			throw new IllegalArgumentException();
		}
	}
	static List<MetroTrack> readParamTrack( Object object ) {
		if ( object instanceof Pair ) {
			return (List)object;
		} else if ( object instanceof MetroTrack ) {
			return Arrays.<MetroTrack>asList((MetroTrack)object);
		} else if ( object instanceof EmptyList ) {
			return Collections.EMPTY_LIST;
		} else {
			throw new IllegalArgumentException( "The value '" + object + "' is not allowed here." );
		}
	}
	static double readParamSyncOffset(Object object) {
		return SchemeUtils.toDouble( object );
	}
	static SyncType readParamSyncType(Object object) {
		object = SchemeUtils.schemeNullCheck(object);
		if ( object == null ) {
			return SyncType.IMMEDIATE;
		} else {
			return SyncType.toSyncType( SchemeUtils.toString( object ) );
		}
	}
	static Procedure readParamProcedure(Object arg) {
		if ( arg  == null ) {
			return null;
		} else if ( arg instanceof Procedure ) {
			return (Procedure) arg;
		} else if ( arg  instanceof Pair ) {
			return new TrackProcedure((Pair)arg);
		} else {
			throw new IllegalArgumentException( "unsupported type of the argument" );
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
				return isOpened();
			}
		});
		SchemeUtils.defineVar( scheme, "open" , new Procedure1("open") {
			@Override
			public Object apply1(Object arg0) throws Throwable {
				open( SchemeUtils.toString( arg0 ) );
				return Invokable.NO_RESULT;
			}
		});
		SchemeUtils.defineVar( scheme, "close" , new ProcedureN("close") {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				close();
				return Invokable.NO_RESULT;
			}
		});
		SchemeUtils.defineVar( scheme, "output" , new ProcedureN("output") {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				for ( Object o : args ) {
					String name = SchemeUtils.toString( o );
					createOutputPort( name );
				}
				return Invokable.NO_RESULT;
			}
		});
		SchemeUtils.defineVar( scheme, "input" , new ProcedureN("input") {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				for ( Object o : args ) {
					String name = SchemeUtils.toString( o );
					createInputPort( name );
				}
				return Invokable.NO_RESULT;
			}
		});
		SchemeUtils.defineVar( scheme, "connect" , new ProcedureN("connect") {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				connectProc(Pulsar.this, args, ConnectProc.CONNECT );
				return Invokable.NO_RESULT;
			}
		});
		SchemeUtils.defineVar( scheme, "disconnect" , new ProcedureN("disconnect") {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				connectProc(Pulsar.this, args, ConnectProc.DISCONNECT );
				return Invokable.NO_RESULT;
			}
		});

		SchemeUtils.defineVar( scheme, "get-all-input" , new ProcedureN("get-all-input") {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				return Pair.makeList( getInputPorts().stream().map( (v)->SchemeUtils.toSchemeString(v) )
					.collect( Collectors.toList() ) );
			}
		});
		SchemeUtils.defineVar( scheme, "get-all-output" , new ProcedureN("get-all-output") {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				return Pair.makeList( getOutputPorts().stream().map( (v)->SchemeUtils.toSchemeString(v) )
					.collect( Collectors.toList() ) );
			}
		});

		SchemeUtils.defineVar( scheme, "set-main" , new ProcedureN("set-main") {
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
		SchemeUtils.defineVar( scheme, "get-main" , new Procedure0("get-main") {
			@Override
			public Object apply0() throws Throwable {
				return getMainProcedure();
			}
		});
		SchemeUtils.defineVar( scheme, "set-cue" , new ProcedureN("set-cue") {
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
		SchemeUtils.defineVar( scheme, "set-playing" , new ProcedureN("set-playing") {
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
		SchemeUtils.defineVar( scheme, "playing?" , new ProcedureN("playing?") {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				return getPlaying();
			}
		});
		SchemeUtils.defineVar( scheme, "play" , new ProcedureN("play") {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				setPlaying( true ); 
				return Invokable.NO_RESULT;
			}
		});
		SchemeUtils.defineVar( scheme, "stop" , new ProcedureN("stop") {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				setPlaying( false ); 
				return Invokable.NO_RESULT;
			}
		});
		SchemeUtils.defineVar( scheme, "quit" , new ProcedureN("quit") {
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
		SchemeUtils.defineVar( scheme, "tapt" , new ProcedureN("tapt") {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				tempoTapper.tap(); 
				return Invokable.NO_RESULT;
			}
		});
		SchemeUtils.defineVar( scheme, "set-tempo" , new ProcedureN("set-tempo") {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				if ( 0 < args.length ) {
					double bpm = SchemeUtils.toDouble(args[0]);
					tempoTapper.setBeatsPerMinute( bpm );
				}

				return Invokable.NO_RESULT;
			}
		});
		// TODO XXX
		SchemeUtils.defineVar( scheme, "get-main-file" , new Procedure0("get-main-file") {
			@Override
			public Object apply0() throws Throwable {
				return SchemeUtils.toSchemeString( getMainFile().getName() );
			}
		});
		// TODO XXX
		SchemeUtils.defineVar( scheme, "set-main-file" , new Procedure1("set-main-file") {
			@Override
			public Object apply1( Object arg ) throws Throwable {
				// XXX
				setMainFile( null, new File( SchemeUtils.toString( arg ) ) );
				return Invokable.NO_RESULT;
			}
		});
		SchemeUtils.defineVar( scheme, "set-related-files" , new ProcedureN("set-related-files") {
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
		SchemeUtils.defineVar( scheme, "get-related-files" , new ProcedureN("get-related-files") {
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
		SchemeUtils.defineVar( scheme, "cue" , new Procedure0( "cue" ) {
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
		Procedure0 resetScheme = new Procedure0( "reset" ) {
			@Override
			public Object apply0() throws Throwable {
				reset();
				return Invokable.NO_RESULT;
			}
		};
		SchemeUtils.defineVar( scheme, "reset" , resetScheme );
		SchemeUtils.defineVar( scheme, "rewind" , new Procedure0("rewind") {
			@Override
			public Object apply0() throws Throwable {
				rewind();
				return Invokable.NO_RESULT;
			}
		});
		
		SchemeUtils.defineVar( scheme, "clear" , new Procedure0( "clear" ) {
			@Override
			public Object apply0() throws Throwable {
				clear();
				return Invokable.NO_RESULT;
			}
		});

		ProcedureN simul = new ProcedureN( "simul" ) {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				synchronized ( getMetroLock() ) {
					try {
						enterTrackChangeBlock();
						for ( int i=0; i<args.length; i++ ) {
							Object arg = args[i];
							if ( arg instanceof Procedure ) {
								((Procedure)arg).apply0();
							} else {
								logWarn( "The value in args[" + i + "] was not a procedure. Ignored. "  );
							}
						}
					} finally {
						leaveTrackChangeBlock();
						notifyTrackChange();
					}
				}
				return Invokable.NO_RESULT;
			}
		};
		SchemeUtils.defineVar( scheme, "simul" , simul );

		ProcedureN getTrack = new ProcedureN( "get-track" ) {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				ArrayList<MetroTrack> t = new ArrayList<>();
				for ( int i=0; i<args.length; i++ ) {
					t.addAll( 
						searchTrack( 
							createInvokable( 
								readParamTrackSearcher( args[i] ))) ) ;
				}
				return Pair.makeList( t );
			}
		};
		SchemeUtils.defineVar( scheme, "gett" , getTrack );
		SchemeUtils.defineVar( scheme, "get-track" , getTrack );

		ProcedureN newTrack = new ProcedureN( "new-track" ) {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				Object name;
				List<Object> tags;
				Procedure procedure;

				switch ( args.length  ){
					case 0 : 
						throw new IllegalArgumentException();
					case 1 :
						name = null;
						tags = null;
						procedure = readParamProcedure(args[0]);
						break;
					case 2 : {
						List<Object> lst = readParamTrackName( args[0] );
						name = lst.remove(0);
						tags = lst;
						procedure = readParamProcedure(args[1]);
						break;
					}	
					default :
						throw new IllegalArgumentException();
				}
				return createTrack( name, tags, procedure );
			}
		};
		SchemeUtils.defineVar( scheme, "newt" , newTrack );
		SchemeUtils.defineVar( scheme, "new-track" , newTrack );
		
		abstract class TrackManagementProcedure extends ProcedureN  {
			TrackManagementProcedure( String name ) {
				super(name);
			}
			abstract void procTrack( List<MetroTrack> trackList, SyncType syncType, MetroTrack syncTrack, double syncOffset );
			
			@Override
			public Object applyN(Object[] args) throws Throwable {
				List<MetroTrack> trackList;
				SyncType syncType;
				List<MetroTrack> syncTrackList;
				double syncOffset;
				switch ( args.length ) {
					case 0 :
						throw new IllegalArgumentException();
					case 1 :
						trackList     = readParamTrack( args[0] );
						syncType      = SyncType.IMMEDIATE;
						syncTrackList = Collections.EMPTY_LIST;
						syncOffset    = 0.0d;
						break;
					case 2 :
						trackList     = readParamTrack( args[0] );
						syncType      = readParamSyncType( args[1] );
						syncTrackList = Collections.EMPTY_LIST;
						syncOffset    = 0.0d;
						break;
					case 3 :
						trackList     = readParamTrack( args[0] );
						syncType      = readParamSyncType( args[1] );
						syncTrackList = readParamTrack( args[2] );
						syncOffset    = 0.0d;
						break;
					case 4 :
						trackList     = readParamTrack( args[0] );
						syncType      = readParamSyncType( args[1] );
						syncTrackList = readParamTrack( args[2] );
						syncOffset    = readParamSyncOffset( args[3] );
						break;
					default :
						throw new IllegalArgumentException();
				}
				
				MetroTrack syncTrack;
				if ( syncTrackList.size() == 0 ) {
					syncTrack = null;
				} else {
					syncTrack = syncTrackList.get(0);
				}
				procTrack( trackList, syncType, syncTrack, syncOffset );
				
				return Invokable.NO_RESULT;
			}
		}

		ProcedureN putTrack = new TrackManagementProcedure( "put-track" ) {
			@Override
			void procTrack( List<MetroTrack> trackList, SyncType syncType, MetroTrack syncTrack, double syncOffset ) {
				putTrack(trackList, syncType, syncTrack, syncOffset);
			}
		};
		SchemeUtils.defineVar( scheme, "putt" ,      putTrack );
		SchemeUtils.defineVar( scheme, "put-track" , putTrack );

		ProcedureN removeTrack = new TrackManagementProcedure( "remove-track" ) {
			@Override
			void procTrack( List<MetroTrack> trackList, SyncType syncType, MetroTrack syncTrack, double syncOffset ) {
				removeTrack(trackList, syncType, syncTrack, syncOffset);
			}
		};
		SchemeUtils.defineVar( scheme, "remt" ,         removeTrack );
		SchemeUtils.defineVar( scheme, "remove-track" , removeTrack );
		

		Procedure notifyTrackChange = new Procedure0( "notify-track-change" ) {
			@Override
			public Object apply0() throws Throwable {
				notifyTrackChange();
				return Invokable.NO_RESULT;
			}
		};
		SchemeUtils.defineVar( scheme, "nott" ,         notifyTrackChange );
		SchemeUtils.defineVar( scheme, "notify-track-change" , notifyTrackChange );

		SchemeUtils.defineVar( scheme, "lst" , new Procedure0("ls") {
			@Override
			public Object apply0() throws Throwable {
				List<MetroTrack> tempAllTracks = replicateAllTracks(); 
				ArrayList<Object> list = new ArrayList<>( tempAllTracks.size() );
				for ( MetroTrack track :  tempAllTracks ) {
					list.add( track );
				}
				Collections.reverse(list);
				return Pair.makeList(list);

			}
		});
		Procedure0 clr = new Procedure0("clear-tracks") {
			@Override
			public Object apply0() throws Throwable {
				clearTracks();
				return Invokable.NO_RESULT;
			}
		};
		SchemeUtils.defineVar( scheme, "clet" , clr);
		SchemeUtils.defineVar( scheme, "clear-tracks" , clr);
		
		SchemeUtils.defineVar( scheme, "print-stack-trace" , new ProcedureN("print-stack-trace") {
			@Override
			public Object apply1(Object arg) throws Throwable {
				((Throwable)arg).printStackTrace();
				return arg;
			}
		});
		SchemeUtils.defineVar( scheme, "display-warn" , new ProcedureN("display-warn") {
			@Override
			public Object apply1(Object arg) throws Throwable {
				System.err.print( arg );
				return Values.empty;
			}
		});
		SchemeUtils.defineVar( scheme, "newline-warn" , new Procedure0("newline-warn") {
			@Override
			public Object apply0() throws Throwable {
				System.err.println();
				return Values.empty;
			}
		});

		SchemeUtils.defineVar( scheme, "list-seq" , new ProcedureN("list-seq") {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				//					logInfo("list-seq");
				List<MetroTrack> tempAllTracks = replicateAllTracks();
				synchronized ( getMetroLock() ) {
					ArrayList<LList> list = new ArrayList<>( tempAllTracks.size() );
					for ( MetroTrack track :  tempAllTracks ) {
						SchemeSequence sequence = (SchemeSequence)track.getSequence();
						list.add( Pair.make( sequence.getTrackName(), sequence.asociationList ));
					}
					return LList.makeList(list);
				}
			}
		});

		SchemeUtils.defineVar( scheme, "typeof" , new ProcedureN("typeof") {
			public Object applyN(Object[] args) throws Throwable {
				if ( 0 < args.length  ) {
					return args[0].getClass().getName();
				} else {
					return Invokable.NO_RESULT;
				}
			}
		});

		SchemeUtils.defineVar( scheme, "mktimer" , new Procedure3("mktimer") {
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

		SchemeUtils.defineVar( scheme, "rnd" , new ProcedureN("rnd") {
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
		
		SchemeUtils.defineVar( scheme, "luck" , new ProcedureN("luck") {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				double probability = args.length == 0 ? 0.5 : SchemeUtils.toDouble( args[0] );
				if ( probability < 0 ) return false;
				if ( 1.0<=probability  ) return true;
				return random.nextBoolean( probability );
			}
		});

		{
			SchemeUtils.execScheme( Pulsar.class, scheme, "lib/init.scm"  );
			SchemeUtils.execScheme( Pulsar.class, scheme, "lib/basic-notes.scm"  );
			SchemeUtils.execScheme( Pulsar.class, scheme, "lib/music.scm"  );
			SchemeUtils.execScheme( Pulsar.class, scheme, "lib/xnoop.scm" );
		}
	}
}
	
