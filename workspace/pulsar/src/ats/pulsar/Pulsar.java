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

import ats.metro.Metro;
import ats.metro.MetroTrack;
import ats.metro.MetroTrack.SyncType;
import ats.pulsar.lib.kawautil.Invocable;
import ats.pulsar.lib.kawautil.InvocableSchemeProcedure;
import ats.pulsar.lib.kawautil.SimpleInvocableSchemeProcedure;
import ats.pulsar.lib.message.queue.SimpleMessageQueue;
import ats.pulsar.lib.swing.MersenneTwisterFast;
import ats.pulsar.lib.swing.SchemeUtils;
import gnu.expr.Language;
import gnu.lists.EmptyList;
import gnu.lists.IString;
import gnu.lists.LList;
import gnu.lists.Pair;
import gnu.mapping.Environment;
import gnu.mapping.Procedure;
import gnu.mapping.ProcedureN;
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
 * <li>main-procedure
 * </ul>
 * <p>
 * The <code>main-file</code> is a file path to the main file which Pulsar is
 * currently referring. Pulsar keeps checking the timestamp of the file and
 * trying to detect file modification. Whenever Pulsar detects any timestamp
 * update on the file, Pulsar automatically reads it and execute.
 * <p>
 * The <code>main-procedure</code> is the procedure which initializes the state
 * of the sequencer. This method is the place where Pulsar starts a new song.
 * <p>
 * If a script file sets <code>main-procedure</code>, this is effectively the
 * application <i>opens</i> a new-file in the sense of general applications. A
 * script file could also leave <code>main-procedure</code> untouched.
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
	/**
	 * Creates an instance of Pulsar object without opening any specific scheme
	 * file. When a user creates an object by this constructor, the sequencer
	 * remains closed after the application boots up. The user must explicitly
	 * open a file to use the application.
	 */
	public Pulsar() {
		this.gui = new PulsarGui( this );
		this.initLanguage();
	}

	/**
	 * Create an instance of Pulsar object and opens the specified scheme file.
	 * @param file a scheme file to open. 
	 */
	public Pulsar( File file ) {
		this();
	}


	/**
	 * The main method which starts up the application. The application opens a file
	 * which is specified in argument values. When more than one arguments were
	 * passed to the method, only the first argument is taken. 
	 * @throws IOException 
	 */
	public static void main(String[] args) throws IOException {
		// ADDED (Mon, 22 Jul 2019 08:32:52 +0900)
		// REMOVED (Mon, 22 Jul 2019 08:43:43 +0900) 
		// NO THIS DOESNT WORK 
		// Compilation.inlineOk = false;
		
		Pulsar pulsar = new Pulsar();
		if ( args.length == 0 ) {
		} else {
			pulsar.openMainFile(null, new File( args[0] ));
		}
	}

//	static void logError( String msg, Throwable e ) {
//        Logger.getLogger(Pulsar.class.getName()).log(Level.SEVERE, msg, e);
////		System.err.println( msg );
//	}
//	static void logInfo( String msg ) {
////        Logger.getLogger(Pulsar.class.getName()).log(Level.INFO, msg);
//		System.err.println( msg );
//	}
	
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

	private Scheme scheme;
	private Language    schemeLanguage;
	private Environment schemeEnvironment;
	
	PulsarGui gui;
	
	private final SimpleMessageQueue<Scheme> messageQueue = new SimpleMessageQueue<Scheme>() {
		@Override
		protected Scheme getResource() {
			return Pulsar.this.getScheme();
		}
	};
	public Invocable createInvocable( Procedure procedure ) {
		return SimpleInvocableSchemeProcedure.createSynchronousllyInvocable( messageQueue, procedure );
	}
	
	public static InvocableSchemeProcedure createInvocable(
			Object syncObj, Environment environment,
			Language language, Procedure procedure) {
		return new InvocableSchemeProcedure(syncObj, environment, language, procedure);
	}

	

	public Scheme getScheme() {
		return scheme;
	}
	public Language getSchemeLanguage() {
		return this.schemeLanguage;
	}
	public Environment getSchemeEnvironment() {
		return schemeEnvironment;
	}
	
	MersenneTwisterFast random = new MersenneTwisterFast( new int[] { 
			(int) System.currentTimeMillis(),
			0x123, 0x234, 0x345, 0x456,
	});
	
	
	/**
	 * This field specifies the procedure to reset all of the states inside the
	 * sequencer and effectively this method starts a song. Whenever a user call
	 * {@link Pulsar#rewind()}, this procedure will be invoked.
	 */
	transient Invocable mainProcedure = null;

	/**
	 * Sets the main-procedure object.
	 * 
	 * @see Pulsar#mainProcedure 
	 */
	public void setMainProcedure( Invocable mainProcedure ) {
		this.mainProcedure = mainProcedure;
	}
	
	/**
	 * Returns the main-procedure object.
	 * 
	 * @see Pulsar#mainProcedure 
	 */
	public Invocable getMainProcedure() {
		return mainProcedure;
	}
	
    /**
	 * This application is designed to implement Variable-Song-Length. Basically the
	 * sequencer is always playing a specific region of a song repeatedly and
	 * remains in the region and only when the user send a "cue" to the sequencer,
	 * the sequencer goes to next region. When users call {@link Pulsar#cue() },
	 * this procedure is invoked. 
	 */
	transient Invocable cueProcedure = null;
	/**
	 * Sets the cue-procedure object.
	 * 
	 * @see Pulsar#cueProcedure 
	 */
	public void setCueProcedure( Invocable cueProcedure ) {
		this.cueProcedure = cueProcedure;
	}
	/**
	 * Returns the cue-procedure object.
	 * 
	 * @see Pulsar#cueProcedure 
	 */
	public Invocable getCueProcedure() {
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

		if ( gui != null ) { 
			if ( this.gui.cb_relatedFiles != null ) {
				this.gui.cb_relatedFiles.removeAllItems();
				for ( File f : this.relatedFiles ) {
					this.gui.cb_relatedFiles.addItem(f.getPath());
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
		if ( this.gui != null ) {
			if ( this.gui.cb_relatedFiles != null ) {
				this.gui.cb_relatedFiles.addItem( f.getPath() );
			}
		}
	}

	/**
	 * Reset the current state of the sequencer except the current opened script
	 * filename. This causes reload the script file.
	 */
	public void reset() {
		initLanguage();
		this.execCleanupHook();
		if ( gui != null )
			this.gui.guiClear();
		this.close();
		this.lastModifiedOfMainFile = NOT_DEFINED;
	}
	
	
    /**
	 * {@link Pulsar#rewind()} method resets the state of the object and calls main
	 * procedure to back to the state of beginning of the project. This method
	 * effectively invoke the main procedure. See {@link Pulsar#mainProcedure}
	 */
    public void rewind() { 
    	logInfo( "===rewind" );
    	setPlaying(false);
    	clearTracks();
    	if ( mainProcedure != null )
    		mainProcedure.invoke();
    }

    /**
     * Invokes cue-procedure. See {@link Pulsar#cueProcedure } 
     */
    public void cue() {
    	logInfo( "===cue" );
    	if ( cueProcedure != null )
    		cueProcedure.invoke();
    }
    
    /**
     * This method clears main procedure and state.
     */
    public void clear() { 
    	logInfo( "===clear" );
    	this.setPlaying(false);
    	this.mainProcedure = null;
    	this.cueProcedure = null;
		clearTracks();
    }
    
    
    final Collection<Runnable> cleanupHook = new LinkedList<>();
    
    /**
     * 
     * @return
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
	static final int PB_POSITION_MAX = 1024;
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
		
		if ( gui != null ) {
			gui.frame.openFile( mainFile );
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
	
	
	// a watchdog Timer
	{
		Timer timer = new Timer(1000, new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				lastModifiedOfConfigFile  = checkLastModified( configFile,  lastModifiedOfConfigFile , (file)->{}   );
				lastModifiedOfMainFile    = checkLastModified( mainFile,    lastModifiedOfMainFile,    (file)->{if ( gui != null ) gui.updateFilename(file);} );
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
						// XXX THIS IS BOGUS
						if ( scheme != null)
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
		SchemeUtils.execSchemeFromFile( scheme, parentFile, file);
	}
	
	private void initLanguage() {
		this.scheme = new Scheme();
		this.schemeEnvironment = this.scheme.getEnvironment();
		this.schemeLanguage = Language.getDefaultLanguage();
		initScheme( scheme );
	}

	/**
	 * Initializes an environment of scheme engine and defines API for the scripts.
	 * 
	 * @param scheme
	 *            the scheme instance to initialize.
	 */
    private void initScheme(Scheme scheme) {
    	{
    		SchemeUtils.execScheme( Pulsar.class, scheme, "init0.scm"  );
    		SchemeUtils.execScheme( Pulsar.class, scheme, "init.scm"  );
    		SchemeUtils.execScheme( Pulsar.class, scheme, "xnoop.scm" );
//    		execScheme( scheme, "event-parser.scm" );
    	}
    	SchemeUtils.defineVar( scheme, "open?" , new ProcedureN() {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				return running;
    		}
    	});
    	SchemeUtils.defineVar( scheme, "open!" , new ProcedureN() {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				for ( Object o : args ) {
					String name = SchemeUtils.toString( o );
					open( name );
				}
				return EmptyList.emptyList;
    		}
    	});
    	SchemeUtils.defineVar( scheme, "close!" , new ProcedureN() {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				close();
				return EmptyList.emptyList;
    		}
    	});
    	SchemeUtils.defineVar( scheme, "output!" , new ProcedureN() {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				for ( Object o : args ) {
					String name = SchemeUtils.toString( o );
					createOutputPort( name );
				}
				return EmptyList.emptyList;
    		}
    	});
    	SchemeUtils.defineVar( scheme, "input!" , new ProcedureN() {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				for ( Object o : args ) {
					String name = SchemeUtils.toString( o );
					createInputPort( name );
				}
				return EmptyList.emptyList;
    		}
    	});
    	SchemeUtils.defineVar( scheme, "connect!" , new ProcedureN() {
			@Override
			public Object applyN(Object[] args) throws Throwable {
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
						connectPort( from, to );
					} catch ( JackException e  ) {
						logError( "" , e );
					}
				}
				return EmptyList.emptyList;
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
					setMainProcedure( Pulsar.createInvocable(getScheme(), getSchemeEnvironment(), getSchemeLanguage(), procedure));
				} else {
					throw new RuntimeException( "invalid argument length" );
				}
    			return EmptyList.emptyList;
    		}
    	});
    	SchemeUtils.defineVar( scheme, "set-cue!" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				logInfo("set-cue");
				if ( args.length == 1 ) {
					Procedure procedure = (Procedure)args[0];
					setCueProcedure( Pulsar.createInvocable( getScheme(), getSchemeEnvironment(), getSchemeLanguage(), procedure ) );
				} else {
					throw new RuntimeException( "invalid argument length" );
				}
    			return EmptyList.emptyList;
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
    			return EmptyList.emptyList;
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
    			return EmptyList.emptyList;
    		}
    	});
    	SchemeUtils.defineVar( scheme, "stop!" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				setPlaying( false ); 
    			return EmptyList.emptyList;
    		}
    	});
    	SchemeUtils.defineVar( scheme, "tap!" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				tempoTapper.tap(); 
    			return EmptyList.emptyList;
    		}
    	});
    	SchemeUtils.defineVar( scheme, "set-tempo!" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				if ( 0 < args.length ) {
					double bpm = SchemeUtils.toDouble(args[0]);
					tempoTapper.setBeatsPerMinute( bpm );
				}
				 
    			return EmptyList.emptyList;
    		}
    	});
    	SchemeUtils.defineVar( scheme, "set-progress-pos!" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				if ( 0 < args.length ) {
					if ( gui != null ) { 
						double value = SchemeUtils.toDouble(args[0]);
						gui.pb_position.setValue((int) (value * PB_POSITION_MAX) );
					}
				}
				 
    			return EmptyList.emptyList;
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
    			return EmptyList.emptyList;
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
    	SchemeUtils.defineVar( scheme, "cue!" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				cue();
    			return EmptyList.emptyList;
    		}
    	});
    	SchemeUtils.defineVar( scheme, "reset!" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				reset();
    			return EmptyList.emptyList;
    		}
    	});
    	SchemeUtils.defineVar( scheme, "rewind!" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				rewind();
    			return EmptyList.emptyList;
    		}
    	});
    	SchemeUtils.defineVar( scheme, "clear!" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				clear();
    			return EmptyList.emptyList;
    		}
    	});

    	SchemeUtils.defineVar( scheme, "put-seq!" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				if ( 2 <= args.length  ) {
					logInfo( "put-seq! : " + Arrays.asList(args).toString() );
					
					String name;
					Collection<String> tags;
					if ( args[0] instanceof Pair ) {
						Pair p = ((Pair)args[0]);
						name = SchemeUtils.symbolToString( p.getCar() );
						
//						Pair.makeList( getInputPorts().stream().map( (v)->SchemeUtils.toSchemeString(v) )
//								.collect( Collectors.toList() ) );

						tags = SchemeUtils.symbolListToStringList(p);
						
					} else {
						name = SchemeUtils.symbolToString( args[0] );
						tags = null;
					}
					
					Procedure procedure     = (Procedure) args[1];
					SyncType syncType       = 3<=args.length ? str2sync( args[2] ) : SyncType.IMMEDIATE;
					String syncTrackName    = 4<=args.length ? SchemeUtils.anyToString( SchemeUtils.schemeNullCheck( args[3] ) ) : null;
					double offset           = 5<=args.length ? SchemeUtils.toDouble( args[4] ) : 0.0d;
					
					SchemeSequence sequence = new SchemeSequence( scheme,
							Pulsar.createInvocable(getScheme(), getSchemeEnvironment(), getSchemeLanguage(),
								procedure) );
					
					putSequence( name, tags, sequence, syncType, syncTrackName, offset );
					 
					return EmptyList.emptyList;
				} else {
					throw new RuntimeException( "Invalid parameter. usage : (put-seq! [name] [lambda] [syncType(immediate|parallel|serial)] [sync-parent-name] [offset] ) " );
				}
			}
			private SyncType str2sync(Object object) {
				return SyncType.valueOf( SchemeUtils.symbolToString( object ).toUpperCase() );
			}
    	});
    	
    	SchemeUtils.defineVar( scheme, "remove-seq!" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				if ( args.length == 1 ) {
					String name = SchemeUtils.toString( args[0] );
					removeTrack(name);
					return EmptyList.emptyList;
				} else {
					throw new RuntimeException( "Invalid parameter. usage : (new-sequence [name] [lambda] ) " );
				}
			}
    	});
    	SchemeUtils.defineVar( scheme, "clear-seq!" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				clearTracks();
				return EmptyList.emptyList;
			}
    	});
    	SchemeUtils.defineVar( scheme, "has-seq?" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				if ( args.length == 1 ) {
					String name = SchemeUtils.toString( args[0] );
					
					return getTrack(name) != null;
				} else {
					throw new RuntimeException( "Invalid parameter. usage : (new-sequence [name] [lambda] ) " );
				}
			}
    	});
    	SchemeUtils.defineVar( scheme, "list-seq" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
//				logInfo("list-seq");
				synchronized ( lock ) {
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
    				return EmptyList.emptyList;
    			}
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

    	if ( gui != null ) {
    		gui.initScheme(scheme);
    	}
    }
}
	
