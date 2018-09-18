package ats.pulsar;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Dictionary;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.function.Consumer;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSlider;
import javax.swing.JTextField;
import javax.swing.SpringLayout;
import javax.swing.Timer;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;
import javax.swing.filechooser.FileFilter;

import org.jaudiolibs.jnajack.JackException;

import ats.metro.Metro;
import ats.metro.MetroInvokable;
import ats.metro.MetroNoteEventBufferSequence;
import ats.metro.MetroNoteEventBufferSequence.SyncType;
import ats.pulsar.lib.FlawLayout;
import ats.pulsar.lib.JNamedPanel;
import ats.pulsar.lib.LayoutUtils;
import ats.pulsar.lib.MersenneTwisterFast;
import ats.pulsar.lib.SpringLayoutUtil;
import gnu.lists.EmptyList;
import gnu.lists.IString;
import gnu.lists.LList;
import gnu.lists.Pair;
import gnu.mapping.Environment;
import gnu.mapping.Procedure;
import gnu.mapping.ProcedureN;
import gnu.mapping.SimpleSymbol;
import gnu.mapping.Symbol;
import gnu.math.DFloNum;
import gnu.math.IntNum;
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
 * @author whatsupAts
 */
public final class Pulsar extends Metro {
	/**
	 * Creates an instance of Pulsar object without opening any specific scheme
	 * file. When a user creates an object by this constructor, the sequencer
	 * remains closed after the application boots up. The user must explicitly
	 * open a file to use the application.
	 */
	public Pulsar() {
	}

	/**
	 * Create an instance of Pulsar object and opens the specified scheme file.
	 * @param file a scheme file to open. 
	 */
	public Pulsar( File file ) {
		if ( ! file.isFile() )
			throw new RuntimeException( "The specified file does not exist (" + file.getPath() + ")" );

		// See the comment.
		setMainFile( null , file );
	}


	/**
	 * The main method which starts up the application. The application opens a file
	 * which is specified in argument values. When more than one arguments were
	 * passed to the method, only the first argument is taken. 
	 */
	public static void main(String[] args) {
		if ( args.length == 0 ) {
			new Pulsar();
		} else {
			new Pulsar( new File( args[0] ) );
		}
	}

	static void logError( String msg, Throwable e ) {
        Logger.getLogger(Pulsar.class.getName()).log(Level.SEVERE, msg, e);
//		System.err.println( msg );
	}
	static void logInfo( String msg ) {
//        Logger.getLogger(Pulsar.class.getName()).log(Level.INFO, msg);
		System.err.println( msg );
	}
	

	Scheme scheme = makeScheme();

	MersenneTwisterFast random = new MersenneTwisterFast( new int[] { 
			(int) System.currentTimeMillis(),
			0x123, 0x234, 0x345, 0x456,
	});
	
	

	static final int BORDER_SIZE = 10;
	
	/**
	 * This field specifies the procedure to reset all of the states inside the
	 * sequencer and effectively this method starts a song. Whenever a user call
	 * {@link Pulsar#rewind()}, this procedure will be invoked.
	 */
	transient MetroInvokable mainProcedure = null;

	/**
	 * Sets the main-procedure object.
	 * 
	 * @see Pulsar#mainProcedure 
	 */
	public void setMainProcedure( MetroInvokable mainProcedure ) {
		this.mainProcedure = mainProcedure;
	}
	
	/**
	 * Returns the main-procedure object.
	 * 
	 * @see Pulsar#mainProcedure 
	 */
	public MetroInvokable getMainProcedure() {
		return mainProcedure;
	}
	
    /**
	 * This application is designed to implement Variable-Song-Length. Basically the
	 * sequencer is always playing a specific region of a song repeatedly and
	 * remains in the region and only when the user send a "cue" to the sequencer,
	 * the sequencer goes to next region. When users call {@link Pulsar#cue() },
	 * this procedure is invoked. 
	 */
	transient MetroInvokable cueProcedure = null;
	/**
	 * Sets the cue-procedure object.
	 * 
	 * @see Pulsar#cueProcedure 
	 */
	public void setCueProcedure( MetroInvokable cueProcedure ) {
		this.cueProcedure = cueProcedure;
	}
	/**
	 * Returns the cue-procedure object.
	 * 
	 * @see Pulsar#cueProcedure 
	 */
	public MetroInvokable getCueProcedure() {
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

		if ( this.cb_relatedFiles != null ) {
			this.cb_relatedFiles.removeAllItems();
			for ( File f : this.relatedFiles ) {
				this.cb_relatedFiles.addItem(f.getPath());
			}
		}
	}
	
	/**
	 * This method is not used now. The developers should not use this method in
	 * order to keep consistency.
	 */
	public void addRelatedFile( File f) {
		this.relatedFiles.add( f );
		if ( this.cb_relatedFiles != null ) {
			this.cb_relatedFiles.addItem( f.getPath() );
		}
	}

	/**
	 * Reset the current state of the sequencer except the current opened script
	 * filename. This causes reload the script file.
	 */
	public void reset() {
		this.scheme = makeScheme();
		this.execCleanupHook();
		this.guiClear();
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
    	clearSequences();
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
		clearSequences();
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
	void setMainFile( File parentFile, File mainFile ) {
		this.mainFile   = mainFile;
		this.parentFile = parentFile;
		this.lastModifiedOfMainFile = NOT_DEFINED;
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
				lastModifiedOfMainFile    = checkLastModified( mainFile,    lastModifiedOfMainFile,    (file)->updateFilename(file) );
			}

			long checkLastModified( File file, long lastModified, Consumer<File> updateProc ) {
				if ( file == null ) 
					return NOT_DEFINED; // lastModified;
				
				long newLastModified = file.lastModified();
				if ( newLastModified != lastModified ) {
					logInfo( "Detected that the file was modified." );
					try {
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
		if ( ! file.isFile() ) {
			throw new FileNotFoundException( file.getPath() );
		}
		
		if ( ! file.isAbsolute() ) {
			if ( parentFile == null )
				throw new FileNotFoundException( "cannot resolve relative path because no parent file is known." );
			
			file = new File( parentFile.getParentFile(), file.getPath() );
		}
		
		try {
			String text = new String(Files.readAllBytes( Paths.get( file.toURI() ) ), StandardCharsets.UTF_8);
			synchronized ( scheme ) {
				scheme.eval( text );
			}
		} catch (IOException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		} catch (Throwable e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
	}
	
	private static final void defineVar( Scheme scheme, String name, Object value ) {
    	scheme.getEnvironment().define( SimpleSymbol.make( "", name ), null, value );
	}
	
	private Scheme makeScheme() {
		Scheme scheme = new Scheme();
		initScheme( scheme );
		return scheme;
	}

	/**
	 * Initializes an environment of scheme engine and defines API for the scripts.
	 * 
	 * @param scheme
	 *            the scheme instance to initialize.
	 */
    private void initScheme(Scheme scheme) {
    	{
    		InputStream in = null;
    		try {
    			in = this.getClass().getResource( "init.scm" ).openStream();
    			scheme.eval( new InputStreamReader( in ) );
    		} catch (Throwable e) {
				new RuntimeException( e );
			} finally {
    			try {
    				if ( in != null)
    					in.close();
				} catch (IOException e) {
					e.printStackTrace();
				}
    		}
    	}
    	defineVar( scheme, "open?" , new ProcedureN() {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				return running;
    		}
    	});
    	defineVar( scheme, "open!" , new ProcedureN() {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				for ( Object o : args ) {
					String name = SchemeUtils.toString( o );
					open( name );
				}
				return EmptyList.emptyList;
    		}
    	});
    	defineVar( scheme, "close!" , new ProcedureN() {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				close();
				return EmptyList.emptyList;
    		}
    	});
    	defineVar( scheme, "output!" , new ProcedureN() {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				for ( Object o : args ) {
					String name = SchemeUtils.toString( o );
					createOutputPort( name );
				}
				return EmptyList.emptyList;
    		}
    	});
    	defineVar( scheme, "input!" , new ProcedureN() {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				for ( Object o : args ) {
					String name = SchemeUtils.toString( o );
					createInputPort( name );
				}
				return EmptyList.emptyList;
    		}
    	});
    	defineVar( scheme, "connect!" , new ProcedureN() {
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

    	defineVar( scheme, "get-all-input" , new ProcedureN() {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				return Pair.makeList( getInputPorts().stream().map((v)->IString.valueOf(v) )
						.collect( Collectors.toList() ) );
    		}
    	});
    	defineVar( scheme, "get-all-output" , new ProcedureN() {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				return Pair.makeList( getOutputPorts().stream().map((v)->IString.valueOf(v) )
						.collect( Collectors.toList() ) );
    		}
    	});

    	defineVar( scheme, "set-main!" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				logInfo("set-main");
				if ( args.length == 1 ) {
					Procedure procedure = (Procedure)args[0];
					setMainProcedure( new InvokableSchemeProcedure( scheme, Environment.getCurrent(), procedure ));
				} else {
					throw new RuntimeException( "invalid argument length" );
				}
    			return EmptyList.emptyList;
    		}
    	});
    	defineVar( scheme, "set-cue!" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				logInfo("set-cue");
				if ( args.length == 1 ) {
					Procedure procedure = (Procedure)args[0];
					setCueProcedure( new InvokableSchemeProcedure( scheme , Environment.getCurrent(), procedure ));
				} else {
					throw new RuntimeException( "invalid argument length" );
				}
    			return EmptyList.emptyList;
    		}
    	});
    	defineVar( scheme, "set-playing!" , new ProcedureN() {
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
    	defineVar( scheme, "playing?" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				return getPlaying();
    		}
    	});
    	defineVar( scheme, "play!" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				setPlaying( true ); 
    			return EmptyList.emptyList;
    		}
    	});
    	defineVar( scheme, "stop!" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				setPlaying( false ); 
    			return EmptyList.emptyList;
    		}
    	});
    	defineVar( scheme, "tap!" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				tempoTapper.tap(); 
    			return EmptyList.emptyList;
    		}
    	});
    	defineVar( scheme, "set-tempo!" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				if ( 0 < args.length ) {
					double bpm = SchemeUtils.toDouble(args[0]);
					tempoTapper.setBeatsPerMinute( bpm );
				}
				 
    			return EmptyList.emptyList;
    		}
    	});
    	defineVar( scheme, "set-related-files!" , new ProcedureN() {
    		@Override
    		public Object applyN(Object[] args) throws Throwable {
    			if ( args.length == 1  ) {
    				Pair p = (Pair)args[0];
    				Collection<File> files=  SchemeUtils.<Object,File>convertList( p, (o)->{
    					return new File( SchemeUtils.anyToString( o ) );
    				});
    				setRelatedFiles( files );
    			} else {
    				throw new RuntimeException( "invalid argument length" );
    			}
    			return EmptyList.emptyList;
    		}
    	});
    	defineVar( scheme, "get-related-files" , new ProcedureN() {
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
    	defineVar( scheme, "cue!" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				cue();
    			return EmptyList.emptyList;
    		}
    	});
    	defineVar( scheme, "reset!" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				reset();
    			return EmptyList.emptyList;
    		}
    	});
    	defineVar( scheme, "rewind!" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				rewind();
    			return EmptyList.emptyList;
    		}
    	});
    	defineVar( scheme, "clear!" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				clear();
    			return EmptyList.emptyList;
    		}
    	});

    	defineVar( scheme, "put-seq!" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				if ( 2 <= args.length  ) {
					logInfo( "put-seq! : " + Arrays.asList(args).toString() );
					
					String name;
					Collection<String> tags;
					if ( args[0] instanceof Pair ) {
						Pair p = ((Pair)args[0]);
						name = SchemeUtils.symbolToString( p.getCar() );
						
						Pair.makeList( getInputPorts().stream().map((v)->IString.valueOf(v) )
								.collect( Collectors.toList() ) );

						tags = SchemeUtils.<Object,String>convertList((Collection<Object>)p.getCdr(), (v)->SchemeUtils.symbolToString(v));
						
					} else {
						name = SchemeUtils.symbolToString( args[0] );
						tags = null;
					}
					
					Procedure procedure     = (Procedure) args[1];
					SyncType syncType       = 3<=args.length ? str2sync( args[2] ) : SyncType.IMMEDIATE;
					String syncSequenceName = 4<=args.length ? SchemeUtils.anyToString( args[3] ) : null;
					double offset           = 5<=args.length ? SchemeUtils.toDouble( args[4] ) : 0.0d;
					
					SchemePulsarLogic logic = new SchemePulsarLogic( scheme,
							new InvokableSchemeProcedure( scheme, Environment.getCurrent(), procedure ) );
					
					putLogic( name, tags, logic, syncType, syncSequenceName, offset );
					 
					return EmptyList.emptyList;
				} else {
					throw new RuntimeException( "Invalid parameter. usage : (put-seq! [name] [lambda] [syncType(immediate|parallel|serial)] [sync-parent-name] [offset] ) " );
				}
			}
			private SyncType str2sync(Object object) {
				return SyncType.valueOf( SchemeUtils.anyToString( object ).toUpperCase() );
			}
    	});
    	defineVar( scheme, "remove-seq!" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				if ( args.length == 1 ) {
					String name = SchemeUtils.toString( args[0] );
					removeLogic(name);
					return EmptyList.emptyList;
				} else {
					throw new RuntimeException( "Invalid parameter. usage : (new-logic [name] [lambda] ) " );
				}
			}
    	});
    	defineVar( scheme, "clear-seq!" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				clearSequences();
				return EmptyList.emptyList;
			}
    	});
    	defineVar( scheme, "has-seq?" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				if ( args.length == 1 ) {
					String name = SchemeUtils.toString( args[0] );
					hasLogic(name);
					return EmptyList.emptyList;
				} else {
					throw new RuntimeException( "Invalid parameter. usage : (new-logic [name] [lambda] ) " );
				}
			}
    	});
    	defineVar( scheme, "list-seq" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
//				logInfo("list-seq");
				synchronized ( lock ) {
					ArrayList<LList> list = new ArrayList<>( sequences.size() );
					for ( MetroNoteEventBufferSequence sequence :  sequences ) {
						SchemePulsarLogic logic = (SchemePulsarLogic)sequence.getLogic();
						list.add( SchemeUtils.acons( logic.getPlayerName(), logic.asociationList ));
					}
					return LList.makeList(list);
				}
			}
    	});
    	defineVar( scheme, "gui-get-pane" , new ProcedureN() {
			// TODO ???
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				logInfo("gui-get-pane");
    			return userPane;
    		}
    	});
    	defineVar( scheme, "gui-get-frame" , new ProcedureN() {
			// TODO ???
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				logInfo("gui-get-frame");
    			return frame;
    		}
    	});
    	defineVar( scheme, "gui-clear!" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				guiClear();
    			return EmptyList.emptyList;
    		}
    	});
    	defineVar( scheme, "gui-pack!" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				guiPack();
    			return EmptyList.emptyList;
    		}
    	});
    	defineVar( scheme, "new-slider" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				if ( args.length == 5 ) {
					int min = ((Number)args[0]).intValue();
					int max = ((Number)args[1]).intValue();
					int minorTick = ((Number)args[2]).intValue();
					int majorTick = ((Number)args[3]).intValue();
					Procedure procedure = (Procedure) args[4];
					JSlider slider = new JSlider() {
						@Override
						public Dimension getPreferredSize() {
							return new Dimension(
									this.getParent().getSize().width,
									super.getPreferredSize().height
									);
							
						}
					};
					
					slider.setMaximum(max);
					slider.setMinimum(min);
					slider.setMajorTickSpacing( majorTick );
					slider.setMinorTickSpacing( minorTick );
					slider.setPaintTicks(true);
					slider.addChangeListener(new ChangeListener() {
						@Override
						public void stateChanged(ChangeEvent e) {
							try {
								procedure.applyN( new Object[] {IntNum.valueOf( slider.getValue() )  } );
							} catch (Throwable e1) {
								logError( "" , e1 );
							}
						}
					});
					return slider;
				} else {
					throw new RuntimeException( "(new-slider min max tick-min tick-maj on-change)" );
				}
			}
    	});

    	defineVar( scheme, "new-button" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				if ( args.length == 3 ) {
						String caption = ((IString) args[0]).toString();
						Object userHandle = args[1];
						Procedure procedure = (Procedure) args[2];

						// Environment env= Environment.getCurrent();
						{
							JButton button = new JButton( caption );
							button.addActionListener( new ActionListener() {
								@Override
								public void actionPerformed(ActionEvent e) {
									try {
										// Environment.setCurrent(env);
										procedure.applyN( new Object[] { userHandle } );
									} catch (Throwable e1) {
										logError( "" , e1 );
									}
								}
							});
							return button;
						}
				} else {
					throw new RuntimeException( "new-button has two parameters( caption user-handle lambda )." );
				}
    		}
    	});
    	defineVar( scheme, "new" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				try {
					return SchemeNewFactory.process(Pulsar.this, args);
				} catch ( Exception e ) {
					logError("", e);
					return null;
				}
    		}
    	});

    	defineVar( scheme, "gui-remove" , new ProcedureN() {
    		@Override
    		public Object applyN(Object[] args) throws Throwable {
    			
    			if ( 1 < args.length ) {
        			ArrayDeque<Object> argList = new ArrayDeque<>( Arrays.asList(args) );
        			Container parent = (Container) SchemeUtils.toNull( argList.pop() );
        			Collection<String> path = SchemeUtils.convertList(argList, (o)->{
    					return SchemeUtils.toString(o);
    				});
    				Component c  = guiRemove( parent, path );
    				return SchemeUtils.toEmptyList( c );
    			} else {
					throw new RuntimeException( 
							"Invalid argument error\n"+
							"usage : (gui-remove [parent] [name])" );
    			}
    		}
    	});

    	defineVar( scheme, "gui-get" , new ProcedureN() {
    		@Override
    		public Object applyN(Object[] args) throws Throwable {
    			if ( 1 < args.length ) {
        			ArrayDeque<Object> argList = new ArrayDeque<>( Arrays.asList(args) );
        			Container parent = (Container) SchemeUtils.toNull( argList.pop() );
        			Collection<String> path = SchemeUtils.convertList(argList, (o)->{
    					return SchemeUtils.toString(o);
    				});
    				Component c  = guiGet( parent, path );
    				return SchemeUtils.toEmptyList( c );
    			} else {
					throw new RuntimeException( 
							"Invalid argument error\n"+
							"usage : (gui-get [parent] [name])" );
    			}
    		}
    	});
    	
    	defineVar( scheme, "gui-build!" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				return guiBuild(args);
    		}
    	});
    	defineVar( scheme, "gui-newline!" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				logInfo("newline-gui");
				if ( args.length == 0 )
					guiNewline(null);
				else
					guiNewline((JComponent) args[0]);
				
    			return EmptyList.emptyList;
    		}
    	});
    	defineVar( scheme, "gui-refresh" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				logInfo("refresh-gui");
				guiRefresh();
    			return EmptyList.emptyList;
    		}
    	});
    	defineVar( scheme, "gui-layout!" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				logInfo( "gui-layout" );
				if ( args.length == 0 ) {
					throw new RuntimeException( "gui-layout! (panel) ['gridbag | 'spring | flow ]" );
				} else if ( args.length == 1 ) {
					guiLayout(userPane, SchemeUtils.symbolToString( args[0] ) );
				} else if ( 2 <= args.length ) {
					guiLayout( (Container) args[0], SchemeUtils.symbolToString( args[1] ) );
				} 
    			return EmptyList.emptyList;
    		}
    	});
    	defineVar( scheme, "gui-gridbag-layout" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				logInfo( "gui-gridbag-layout" );
				guiGridBagLayout(userPane);
    			return EmptyList.emptyList;
    		}
    	});
    	defineVar( scheme, "gui-spring-layout" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				logInfo( "gui-spring-layout" );
				guiSpringLayout(userPane);
    			return EmptyList.emptyList;
    		}
    	});
    	defineVar( scheme, "gui-flow-layout" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				logInfo( "gui-flow-layout" );
				guiFlowLayout(userPane);
    			return EmptyList.emptyList;
    		}
    	});
    	defineVar( scheme, "gui-put-constraint" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				logInfo("refresh-gui");
			 	SpringLayout springLayout = ((SpringLayout)userPane.getLayout());
				if ( args.length == 5 ) {
					new SpringLayoutUtil(springLayout, userPane).putConstraint( args[0],args[1],args[2],args[3],args[4]  );
				} else if ( args.length == 4 ) {
					new SpringLayoutUtil(springLayout, userPane).putConstraint( args[0],args[1], 5,     args[2], args[3] );
				} else {
					throw new RuntimeException( "put-constraint has five parameters( constraint1 component1 pad constraint2 component2  )." );
				}
				guiRefresh();
    			return EmptyList.emptyList;
    		}
    	});
    	defineVar( scheme, "rnd" , new ProcedureN() {
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
    	defineVar( scheme, "luck" , new ProcedureN() {
    		@Override
    		public Object applyN(Object[] args) throws Throwable {
    			double probability = args.length == 0 ? 0.5 : SchemeUtils.toDouble( args[0] );
    			if ( probability < 0 ) return false;
    			if ( 1.0<=probability  ) return true;
				return random.nextBoolean( probability );
    		}
    	});

    }
    
    ////////////////////////

	//Create the "cards".
    JFrame frame = null;
    Container rootPane = null; 
	JPanel staticPane = new JPanel();

	JPanel userPane = new JNamedPanel( new FlawLayout() );

    transient boolean isComboBoxUpdating = false;
	JComboBox<String> cb_relatedFiles;
	JTextField tf_currentFile;
	
	public void guiClear() {
		userPane.removeAll();
		guiFlowLayout(userPane);
		// frame.pack();
	}
	public void guiPack() {
		frame.pack();
	}
	public void guiFlowLayout(Container userPane) {
		userPane.setLayout( new FlawLayout( FlawLayout.LEFT, 2, 2 ) );
	}
	public void guiBorderLayout(Container userPane) {
		userPane.setLayout( new BorderLayout( BORDER_SIZE,BORDER_SIZE ) );
	}
	public void guiSpringLayout( Container userPane ) {
		userPane.setLayout( new SpringLayout() );
	}
	public void guiGridBagLayout(Container userPane) {
		userPane.setLayout( new GridBagLayout() );
	}
	public void guiLayout( Container container, String type ) {
		switch ( type ) {
			case "default":
			case "flow" :
				guiFlowLayout( container );
				break;
			case "border" :
				guiBorderLayout( container );
				break;
			case "spring" :
				guiSpringLayout( container );
				break;
			case "gridbag" :
				guiGridBagLayout( container );
				break;
			default :
				throw new RuntimeException( "Unknown LayoutManager name : " + type );
		}
	}
	
	public void guiRefresh() {
		userPane.revalidate();
		userPane.repaint();
        // frame.pack();
	}
	public Component guiResolve( Container parent, Collection<String> path, boolean errorIfNotFound ) {
		if ( parent == null )
			parent = userPane;
		
		Component curr = parent;
		
		List<String> tracePath = new ArrayList<>();
		for ( Iterator<String> i=path.iterator(); i.hasNext(); ) {
			String name = i.next();
			tracePath.add( name );

			if ( curr instanceof JNamedPanel ) {
				Component c = ((JNamedPanel)curr).getComponentByName( name );
				if ( c== null ) {
					if ( errorIfNotFound )
						throw new RuntimeException( name + " is not found : ( " + tracePath.toString() + " )" );
					else 
						return null;
				} else {
					curr = c;
				}
			} else {
				if ( errorIfNotFound )
					throw new RuntimeException( "The passed object is not a named container object." );
				else
					return null;
			}
		}
		return curr;
		
	}
	public Component guiRemove( Container parent, Collection<String> path ) {
		Component c = guiResolve( parent, path, false );
		parent.remove( c );
		return c;
	}
	
	public Component guiGet( Container parent, Collection<String> path ) {
		return guiResolve( parent, path,  false );
	}

	public void guiAdd( Container parent, Component c, Object constraint ) {
		if ( parent == null )
			parent = userPane;
		parent.add( c, constraint );
	}
	public void guiAdd( Container parent, Component c  ) {
		if ( parent == null )
			parent = userPane;
		parent.add( c  );
	}
	public Object guiBuild(Object[] args) {
		Container parent;
		if ( args[0] instanceof Container )
			parent = (Container) args[0];
		else if ( args[0] instanceof EmptyList ) { 
			parent = null;
		} else {
			throw new RuntimeException( "An invalid parent object was specified. " );
		}
		
		String mode = null;
		for ( int i=1; i<args.length; i++ ) {
			Object curr = args[i];
			if ( curr instanceof Symbol ) {
				String symbolName = SchemeUtils.symbolToString( curr );
				switch ( symbolName ) {
					case "newline" : 
						guiNewline( parent );
						break;
					case "list":
						mode = "list";
						break;
					case "name":
						mode = "name";
						break;
					case "label":
						mode = "label";
						break;
					default :
						throw new RuntimeException( "gui-build! unknown type \"" + symbolName + "\"" );
				}
				// mode = null;
			} else if ( curr instanceof IString ) {
				if ( "label".equals( mode ) ) {
					guiAdd( parent, (Component) SchemeNewFactory.process(
							this,
							Symbol.makeUninterned("label"), 
							SchemeUtils.anyToString( curr ) ) );
					
					mode = null;
				} else if ( "name".equals( mode ) ) {
					guiName( parent, SchemeUtils.toString( curr ) ); 
					mode = null;
				} else {
					throw new RuntimeException( "An invalid state was detected " + mode );
				}
			} else {
				if ( curr instanceof Pair  ) {
					Pair p = (Pair) curr;
					if ( "list".equals( mode ) ) {
						for ( Object e : p ) {
							guiAdd( parent, (Component) e );
						}
					} else {
						Object car = p.getCar();
						Object cdr = p.getCdr();
						
						if ( cdr instanceof Pair ) {
							guiAdd( parent, (Component)car, LayoutUtils.map2constraint( cdr ) );
						} else {
							guiAdd( parent, (Component)car, cdr );
						}
					}
				} else {
					guiAdd( parent, (Component)curr );
				}
				mode = null;
			}
		}
		return parent;
	}	
	
	public void guiName( Container parent, String name ) {
		if ( parent == null )
			parent = userPane;
		
		if ( parent instanceof JNamedPanel ) {
			((JNamedPanel)parent).setNextComponentName( name );
		}
	}
	public void guiNewline( Container parent ) {
		if ( parent == null )
			parent = userPane;
		parent.add( FlawLayout.createNewLine() );
	}
	
	public void updateFilename(File file) {
		if ( tf_currentFile != null ) {
			tf_currentFile.setText( file.getPath() );
		}
		
		if ( cb_relatedFiles != null ) {
			int i = this.relatedFiles.indexOf( file );
			if ( 0<=i ) {
				if ( ! isComboBoxUpdating  ) {
					try { 
						isComboBoxUpdating = true;
						this.cb_relatedFiles.setSelectedIndex( i );
					} finally {
						isComboBoxUpdating = false;
					}
				}
			}
		}
	}

	public void newlineGui() {
		// userPane.add( Box.createVerticalStrut(500 ) );
	}

	/**
	 * Initialize GUI 
	 */
	{
		javax.swing.SwingUtilities.invokeLater( new Runnable() {
			public void run() {
				createAndShowGUI();
			}
		});
	}

    private void createAndShowGUI() {
        //Create and set up the window.
        frame = new JFrame( "Pulsar" );
        
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        //Create and set up the content pane.

        
        rootPane = new JPanel( new BorderLayout() );
		rootPane.add( staticPane, BorderLayout.PAGE_START );
		rootPane.add( userPane, BorderLayout.CENTER );
		staticPane.setLayout( new BorderLayout(BORDER_SIZE,BORDER_SIZE) );
        frame.getContentPane().add ( rootPane );

		// Tempo Button
        staticPane.add( new JPusarFilePanel(), BorderLayout.PAGE_START );
		staticPane.add( createStartStopButton(), BorderLayout.LINE_END );
		staticPane.add( createTempoTapButton(), BorderLayout.CENTER );
		staticPane.add( createRewindButton(), BorderLayout.LINE_START );
		staticPane.add( createCueButton(), BorderLayout.PAGE_END );

		// createEmptyBorder( top left bottom right )
		staticPane.setBorder( BorderFactory.createEmptyBorder(10,20,5,20) );
		
		userPane.setBorder(   BorderFactory.createEmptyBorder(5,20,20,20) );

		((JComponent)rootPane).setBorder( BorderFactory.createEmptyBorder() );

		// frame.setMaximizedBounds(new Rectangle(0, 0, 400, 1000));
		frame.pack();
		frame.setSize( 600, 500 );
		frame.setVisible(true);
    }
    /*
     * === Meaning of these variable name for panels ===
     * ex) panel_p0_1_2
     * These numbers denote its position.
     *    ___________________
     *    |                 |
     *    |        1        |
     *    |_________________|
     *    |   |         |   |
     *    | 4 |    0    | 2 |
     *    |___|_________|___|
     *    |                 |
     *    |        3        |
     *    |_________________|
     * 
     * The variable name "panel_p0_1_2" denotes that 
     * it is the panel on the position No.2 inside the
     * panel on the position No.1 inside the panel on 
     * the position No.0 .
     * 
     */

    public static BorderLayout newLayout() {
    	return new BorderLayout( BORDER_SIZE, BORDER_SIZE );
    }

    class JPusarFilePanel extends JPanel {
    	public JPusarFilePanel() {
    		super( newLayout() );
    	}
    	JPanel panel_outer = new JPanelExtentionOuter();
    	{
    		add( panel_outer, BorderLayout.CENTER );
    	}

    	class JPanelExtentionOuter extends JPanel {
    		public JPanelExtentionOuter() {
    			super( newLayout() );
    		}

        	JPanel panel_exec = new JPanelExtentionExec();
        	{
        		add( panel_exec, BorderLayout.CENTER );
        	}
    		class JPanelExtentionExec extends JPanel {
    			public JPanelExtentionExec() {
    				super( newLayout() );
    			}
    			JButton execButton = new JButton( "EXEC" ) {
    				@Override
    				public Dimension getPreferredSize() {
    					Dimension s = super.getPreferredSize();
    					s.width = 75;
    					return s;
    				}
    			};
    			{
    				execButton.addActionListener( new ActionListener() {
    					@Override
    					public void actionPerformed(ActionEvent e) {
    						int i = cb_relatedFiles.getSelectedIndex();
    						if ( 0<=i ) {
    							File file = relatedFiles.get(i);
    							setMainFile( relatedFileParent, file );
    						}
    					}
    				});
    				add( execButton , BorderLayout.LINE_START );
    			}

    			JComboBox<String> cb_relatedFiles = new JComboBox<String>() {
    			};
    			{
    				Pulsar.this.cb_relatedFiles = cb_relatedFiles;
    				cb_relatedFiles.setEditable(false);
    				cb_relatedFiles.addPopupMenuListener( new PopupMenuListener() {
    					@Override
    					public void popupMenuWillBecomeVisible(PopupMenuEvent e) {
    						logInfo("popupMenuWillBecomeVisible()");
    						// readHistoryFile(comboBox);
    					}
    					@Override
    					public void popupMenuWillBecomeInvisible(PopupMenuEvent e) {
    						logInfo( "popupMenuWillBecomeInvisible()");
    					}
    					@Override
    					public void popupMenuCanceled(PopupMenuEvent e) {
    						logInfo(".popupMenuCanceled()");
    					}
    				});
    				cb_relatedFiles.addItemListener( new ItemListener() {
    					@Override
    					public void itemStateChanged(ItemEvent e) {
    						logInfo("Pulsar.createFilePanel().new ItemListener() {...}.itemStateChanged()");
    						if ( e.getStateChange() == ItemEvent.SELECTED ) {
    							// Do nothing.
    						}
    					}
    				});
    				add( cb_relatedFiles, BorderLayout.CENTER );
    			}
    		}

    		JPanel panel_openFile = new JOpenFilePanel();
        	{
        		add( panel_openFile, BorderLayout.PAGE_START );
        	}
        	class JOpenFilePanel extends JPanel {
        		JOpenFilePanel(){ super( newLayout() ); }

        		JButton openMainFileButton = new JButton( "OPEN" );
        		{	
        			this.add( openMainFileButton, BorderLayout.LINE_START );
        			openMainFileButton.addActionListener(new ActionListener() {
        				@Override
        				public void actionPerformed(ActionEvent e) {
        					JFileChooser fc = new JFileChooser();
        					fc.setFileFilter( new FileFilter() {
        						@Override
        						public String getDescription() {
        							return "*.scm (scheme)";
        						}
        						@Override
        						public boolean accept(File f) {
        							return ! f.isFile() || f.getName().endsWith( "scm" );
        						}
        					});
        					int result = fc.showOpenDialog( frame );
        					if ( result == JFileChooser.APPROVE_OPTION ) {
        						close();
        						setMainFile( null, fc.getSelectedFile() );
        					}
        				}
        			});
        		}
        		JTextField currentFile = new JTextField();
        		{
        			tf_currentFile = currentFile;
        			this.add( currentFile, BorderLayout.CENTER );
        			currentFile.setEditable(false);
        		}
        	}
    	}
    	JButton resetButton = new JButton( "⟳" ) {
    		@Override
    		public Dimension getPreferredSize() {
    			return new Dimension( 75,super.getPreferredSize().height + 20 );
    		}
    	};
    	{
    		add( resetButton , BorderLayout.LINE_END );
    		resetButton.addActionListener( new ActionListener() {
    			@Override
    			public void actionPerformed(ActionEvent e) {
    				reset();
    			}
    		});
    	}

    	

//    	ImageIcon resetIcon;
//    	{
//    		resetIcon = loadIcon( "reset.png" );
//    	}
//		public ImageIcon loadIcon( String path )  {
//			try {
//				return new ImageIcon( ImageIO.read(getClass().getResource( path )) );
//			} catch (IOException e) {
//				logError("", e);
//				return null;
//			}
//		}
		
		

		JSlider sl_tempoSlider = new JSlider();
		{
			add( sl_tempoSlider, BorderLayout.SOUTH );
			sl_tempoSlider.setMinimum(1);
			sl_tempoSlider.setMaximum(1000);
			sl_tempoSlider.setPaintTicks(true);
			sl_tempoSlider.setPaintTrack( true);
			sl_tempoSlider.setMajorTickSpacing(100);
			sl_tempoSlider.setMinorTickSpacing(25);
			Dictionary<Integer,JLabel> labelTables = new Hashtable<>();
			labelTables.put(10, new JLabel( "10" ));
			labelTables.put(50, new JLabel( "50" ));
			labelTables.put(100, new JLabel( "100" ));
			labelTables.put(150, new JLabel( "150" ));
			labelTables.put(200, new JLabel( "200" ));
			labelTables.put(300, new JLabel( "300" ));
			labelTables.put(400, new JLabel( "400" ));
			labelTables.put(500, new JLabel( "500" ));
			labelTables.put(750, new JLabel( "750" ));
			labelTables.put(1000, new JLabel( "1000" ));

			sl_tempoSlider.setLabelTable( labelTables );
			sl_tempoSlider.setPaintLabels(true);
			sl_tempoSlider.addChangeListener(new ChangeListener() {
				@Override
				public void stateChanged(ChangeEvent e) {
					try {
						//    					logInfo( "TempoSlider : " + ((JSlider)e.getSource()).getValue() );
						tempoTapper.setBeatsPerMinute( ((JSlider)e.getSource()).getValue() );
					} catch (JackException e1) {
						logError("", e1);
					}
				}
			});

			tempoTapper.registerNotifier(new TempoTapperTempoNotifier() {
				@Override
				public void notifyTempo(double beatPerMinute) {
					sl_tempoSlider.setValue( (int)beatPerMinute );
				}
			});
		}
    }
    
    private JButton createStartStopButton() {
    	JButton b = new JButton( "■|▶" ) {
    		@Override
    		public Dimension getPreferredSize() {
    			Dimension s = super.getPreferredSize();
    			s.width =  75;
    			return s;
    		}
    	};
    	b.addActionListener( new ActionListener() {
    		@Override
    		public void actionPerformed(ActionEvent e) {
    			togglePlaying();
    		}
    	});
    	return b;
    }
    
    private JButton createRewindButton() {
    	JButton b = new JButton( "||◀" ) {
    		@Override
    		public Dimension getPreferredSize() {
    			Dimension s = super.getPreferredSize();
    			s.width =  75;
    			return s;
    		}
    	};
    	b.addActionListener( new ActionListener() {
    		@Override
    		public void actionPerformed(ActionEvent e) {
    			rewind();
    		}
    	});
    	return b;
    }

    private JButton createCueButton() {
    	JButton b = new JButton( "=== CUE ===" ) {
    		@Override
    		public Dimension getPreferredSize() {
    			return new Dimension( super.getPreferredSize().width, 100 );
    		}
    	};
    	b.addActionListener( new ActionListener() {
    		@Override
    		public void actionPerformed(ActionEvent e) {
    			cue();
    		}
		});
		return b;
	}
    
	private JButton createTempoTapButton() {
    	JButton tempoTapButton = new JButton( "TEMPO" );
    	tempoTapButton.addActionListener( new ActionListener() {
    		@Override
    		public void actionPerformed(ActionEvent e) {
				tempoTapper.tap();

    		}
    	});

    	tempoTapper.registerNotifier( new TempoTapperTempoNotifier() {
			@Override
			public void notifyTempo(double beatPerMinute) {
				tempoTapButton.setText( String.format( "Tempo=%.2f", beatPerMinute  ) );
			}
		} );

    	tempoTapButton.setPreferredSize(new Dimension(200, 100));
    	tempoTapButton.setMargin(new Insets(20, 20, 20, 20));

    	return tempoTapButton;
	}
}
	
