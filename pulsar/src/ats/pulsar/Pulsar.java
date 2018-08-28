package ats.pulsar;

import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.LayoutManager;
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
import java.util.Collections;
import java.util.List;
import java.util.function.Consumer;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SpringLayout;
import javax.swing.Timer;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;
import javax.swing.filechooser.FileFilter;

import org.jaudiolibs.jnajack.JackException;

import ats.metro.Metro;
import ats.metro.MetroNoteEventBufferSequence.SyncType;
import ats.pulsar.lib.FlawLayout;
import ats.pulsar.lib.LayoutUtils;
import ats.pulsar.lib.SpringLayoutUtil;
import gnu.lists.EmptyList;
import gnu.lists.IString;
import gnu.lists.Pair;
import gnu.mapping.Environment;
import gnu.mapping.Procedure;
import gnu.mapping.ProcedureN;
import gnu.mapping.SimpleSymbol;
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
	}
	static void logInfo( String msg ) {
        Logger.getLogger(Pulsar.class.getName()).log(Level.INFO, msg);
	}
	

	static final int BORDER = 10;
	
	/**
	 * This field specifies the procedure to reset all of the states inside the
	 * sequencer and effectively this method starts a song. Whenever a user call
	 * {@link Pulsar#reset()}, this procedure will be invoked.
	 */
	Runnable mainProcedure = null;

	/**
	 * Sets the main-procedure object.
	 * 
	 * @see Pulsar#mainProcedure 
	 */
	public void setMainProcedure( Runnable mainProcedure ) {
		this.mainProcedure = mainProcedure;
	}
	
	/**
	 * Returns the main-procedure object.
	 * 
	 * @see Pulsar#mainProcedure 
	 */
	public Runnable getMainProcedure() {
		return mainProcedure;
	}
	
    /**
	 * This application is designed to implement Variable-Song-Length. Basically the
	 * sequencer is always playing a specific region of a song repeatedly and
	 * remains in the region and only when the user send a "cue" to the sequencer,
	 * the sequencer goes to next region. When users call {@link Pulsar#cue() },
	 * this procedure is invoked. 
	 */
	Runnable cueProcedure = null;
	/**
	 * Sets the cue-procedure object.
	 * 
	 * @see Pulsar#cueProcedure 
	 */
	public void setCueProcedure( Runnable cueProcedure ) {
		this.cueProcedure = cueProcedure;
	}
	/**
	 * Returns the cue-procedure object.
	 * 
	 * @see Pulsar#cueProcedure 
	 */
	public Runnable getCueProcedure() {
		return cueProcedure;
	}

	
	/**
	 * Stores the path of the file which modified the related-file-list property.
	 * This path is used to resolve a relative path by
	 * {@link Pulsar#loadScheme(File) } method.
	 */
	File relatedFileParent=null;

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
	public List<File> getRelatedFiles() {
		return Collections.unmodifiableList( relatedFiles );
	}

	/**
	 * @see #relatedFiles
	 */
	public void setRelatedFiles( List<File> fileList ) {
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
	 * {@link Pulsar#reset()} method resets the state of the object and calls main
	 * procedure to back to the state of beginning of the project. This method
	 * effectively invoke the main procedure. See {@link Pulsar#mainProcedure}
	 */
    public void reset() { 
    	logInfo( "===reset" );
    	setPlaying(false);
    	if ( mainProcedure != null )
    		mainProcedure.run();
		clearSequences();
    }

    /**
     * Invokes cue-procedure. See {@link Pulsar#cueProcedure } 
     */
    public void cue() {
    	logInfo( "===cue" );
    	if ( cueProcedure != null )
    		cueProcedure.run();
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
	 * A {@link Runnable} derived object to execute scheme functions.  
	 */
	static final class SchemeProcedureRunnable implements Runnable {
		private Environment environment;
		private final Procedure procedure;
		SchemeProcedureRunnable(Procedure procedure) {
			this( Environment.getCurrent(), procedure );
		}
		SchemeProcedureRunnable(Environment environment, Procedure procedure) {
			this.environment = environment;
			this.procedure = procedure;
		}
		@Override
		public void run() {
			try {
				Environment.setCurrent( this.environment );
				procedure.applyN( new Object[] {} );
			} catch (Throwable e) {
				logError( "" , e );
				throw new RuntimeException(e);
			}
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
			Scheme scheme = new Scheme();
			initScheme( scheme );
			scheme.eval( text );
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
    	defineVar( scheme, "set-main!" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				logInfo("set-main");
				if ( args.length == 1 ) {
					Procedure procedure = (Procedure)args[0];
					setMainProcedure( new SchemeProcedureRunnable( procedure ));
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
					setCueProcedure( new SchemeProcedureRunnable( procedure ));
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
    	defineVar( scheme, "set-related-files!" , new ProcedureN() {
    		@Override
    		public Object applyN(Object[] args) throws Throwable {
    			if ( args.length == 1  ) {
    				Pair p = (Pair)args[0];
    				List<File> files=  SchemeUtils.<File>convList( p, (o)->{
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
					String name             = SchemeUtils.toString( args[0] );
					Procedure procedure     = (Procedure) args[1];
					SyncType syncType       = 3<=args.length ? str2sync( args[2] ) : SyncType.IMMEDIATE;
					String syncSequenceName = 4<=args.length ? SchemeUtils.anyToString( args[3] ) : null;
					double offset           = 5<=args.length ? SchemeUtils.toDouble( args[4] ) : 0.0d;
					
					SchemePulsarLogic logic = new SchemePulsarLogic( Environment.getCurrent(), procedure );
					putLogic(name, logic, syncType, syncSequenceName, offset );
					
					return EmptyList.emptyList;
				} else {
					throw new RuntimeException( "Invalid parameter. usage : (new-seq [name] [lambda] ) " );
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
    	defineVar( scheme, "new-button" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				if ( args.length == 3 ) {
					if ( args[2] instanceof ProcedureN ) {
						String caption = ((IString) args[0]).toString();
						Object userHandle = args[1];
						ProcedureN procedure = (ProcedureN) args[2];
						Environment env= Environment.getCurrent();
						{
							JButton button = new JButton( caption );
							button.addActionListener( new ActionListener() {
								@Override
								public void actionPerformed(ActionEvent e) {
									try {
										Environment.setCurrent(env);
										procedure.applyN( new Object[] { userHandle } );
									} catch (Throwable e1) {
										logError( "" , e1 );
									}
								}
							});
							return button;
						}
					} else {
						throw new RuntimeException( "add-button unsupported type of args[2]." + SchemeUtils.className( args[2] ) );
					}
				} else {
					throw new RuntimeException( "add-button has two parameters( caption user-handle lambda )." );
				}
    		}
    	});
    	defineVar( scheme, "gui-add!" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				if ( args.length == 1 ) {
					userPane.add( (JComponent)args[0] );
				} else if ( args.length == 2 ) {
					userPane.add( (JComponent)args[0], LayoutUtils.map2constraint( args[1] ) );
				}
    			return args[0];
    		}
    	});
    	defineVar( scheme, "gui-newline!" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				logInfo("newline-gui");
				guiNewline();
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
    	defineVar( scheme, "gui-gridbag-layout" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				logInfo( "gui-gridbag-layout" );
				guiGridBagLayout();
    			return EmptyList.emptyList;
    		}
    	});
    	defineVar( scheme, "gui-spring-layout" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				logInfo( "gui-spring-layout" );
				guiSpringLayout();
    			return EmptyList.emptyList;
    		}
    	});
    	defineVar( scheme, "gui-flow-layout" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				logInfo( "gui-flow-layout" );
				guiFlowLayout();
    			return EmptyList.emptyList;
    		}
    	});
    	defineVar( scheme, "gui-put-constraint" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				logInfo("refresh-gui");
			 	SpringLayout springLayout = ((SpringLayout)userLayout);
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
    }
    
    ////////////////////////

	//Create the "cards".
    JFrame frame = null;
    Container rootPane = null; 
	JPanel staticPane = new JPanel();

	LayoutManager userLayout = null; 
	JPanel userPane = new JPanel( new FlawLayout() );

    transient boolean isComboBoxUpdating = false;
	JComboBox<String> cb_relatedFiles;
	JTextField tf_currentFile;
	
	public void guiClear() {
		userPane.removeAll();
		guiFlowLayout();
	}
	public void guiSpringLayout() {
		userLayout = new SpringLayout();
		userPane.setLayout(userLayout);
	}
	public void guiFlowLayout() {
		userLayout = new FlawLayout( FlawLayout.LEFT, 2, 2 );
		userPane.setLayout(userLayout);
	}
	public void guiGridBagLayout() {
		userLayout = new GridBagLayout();
		userPane.setLayout(userLayout);
	}
	public void guiRefresh() {
		userPane.revalidate();
		userPane.repaint();
        // frame.pack();
	}
	public void guiAdd( JComponent c ) {
		userPane.add( c );
		userPane.revalidate();
		userPane.repaint();
	}
	public void guiNewline() {
		userPane.add( FlawLayout.createNewLine() );
		userPane.revalidate();
		userPane.repaint();
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
		staticPane.setLayout( new BorderLayout(BORDER,BORDER) );
        frame.getContentPane().add ( rootPane );

		// Tempo Button
        staticPane.add( createFilePanel(), BorderLayout.PAGE_START );
		staticPane.add( createStartStopButton(), BorderLayout.LINE_START );
		staticPane.add( createTempoTapButton(), BorderLayout.CENTER );
		staticPane.add( createResetButton(), BorderLayout.LINE_END );
		staticPane.add( createCueButton(), BorderLayout.PAGE_END );

		 staticPane.setBorder( BorderFactory.createEmptyBorder(20,20,20,20) );
		
		((JComponent)rootPane).setBorder( BorderFactory.createEmptyBorder() );

		// frame.setMaximizedBounds(new Rectangle(0, 0, 400, 1000));
		frame.pack();
		frame.setSize( 600, 500 );
		frame.setVisible(true);
    }

	private JPanel createFilePanel() {
		JPanel panel = new JPanel();
		panel.setLayout( new BorderLayout(BORDER,BORDER) );
		JButton execButton = new JButton( "EXEC" ) {
			@Override
			public Dimension getPreferredSize() {
				Dimension s = super.getPreferredSize();
				s.width = 75;
				return s;
			}
		};
		this.cb_relatedFiles = new JComboBox<String>() {;
		};
		cb_relatedFiles.setEditable(false);

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
		
		panel.add( execButton , BorderLayout.LINE_START );

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
		
		cb_relatedFiles.addItemListener(new ItemListener() {
			@Override
			public void itemStateChanged(ItemEvent e) {
				logInfo("Pulsar.createFilePanel().new ItemListener() {...}.itemStateChanged()");
				if ( e.getStateChange() == ItemEvent.SELECTED ) {
					// Do nothing.
				}
			}
		});
		
		panel.add( cb_relatedFiles, BorderLayout.CENTER );
		
		JPanel mainFilePanel = new JPanel();
		panel.add( mainFilePanel, BorderLayout.PAGE_START );
		mainFilePanel.setLayout( new BorderLayout( BORDER , BORDER ) );
		JButton openMainFileButton = new JButton( "OPEN" );
		mainFilePanel.add( openMainFileButton, BorderLayout.LINE_START );
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
					setMainFile( null, fc.getSelectedFile() );
				}
			}
		});

		tf_currentFile = new JTextField();
		mainFilePanel.add( tf_currentFile, BorderLayout.CENTER );
		tf_currentFile.setEditable(false);
		

		return panel;
	}
    
	private JButton createStartStopButton() {
		JButton b = new JButton( "<</II" ) {
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
	private JButton createResetButton() {
		JButton b = new JButton( "RESET" );
		b.addActionListener( new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				reset();
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

    		@Override
    		public void actionPerformed(ActionEvent e) {
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
    					double beatPerMinute =  onemin / avg  ;
    					logInfo( String.format( "%.2f / %.2f = %.2f", onemin , avg , beatPerMinute  ) );
    					setBeatsPerMinute( (long) beatPerMinute );
    					tempoTapButton.setText( String.format( "Tempo=%.2f", beatPerMinute  ) );

    					//							tempoLabel.setText( String.format( "%.2f", beatPerMinute  ) );
    					//							tempoLabel.setAlignmentX(0);
    					//							tempoLabel.setAlignmentY(0);
    				} catch (JackException e1) {
    					// TODO Auto-generated catch block
    					e1.printStackTrace();
    				}
    		}
    	});

    	tempoTapButton.setPreferredSize(new Dimension(200, 100));
    	tempoTapButton.setMargin(new Insets(20, 20, 20, 20));

    	return tempoTapButton;
	}
}
	
