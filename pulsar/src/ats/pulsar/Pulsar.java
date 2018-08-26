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
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SpringLayout;
import javax.swing.SwingConstants;
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

public final class Pulsar extends Metro {
	public Pulsar( File currentFile ) throws JackException {
		if ( ! currentFile.isFile() )
			throw new RuntimeException( "The specified file does not exist (" + currentFile.getPath() + ")" );

		// See the comment.
		setCurrentFile( null , currentFile );
	}
	public Pulsar() {
	}


	/**
	 * timer frequently check the file that the {@link Pulsar#currentFile} points if its
	 */
	public static void main(String[] args) throws JackException, InterruptedException {
		if ( args.length == 0 ) {
			System.err.println( "Usage : pulsar [.scm]" );
			return;
		}
		
		Pulsar pulsar = new Pulsar( new File( args[0] ) );
		
//		pulsar.open( "Metro" );
//
//		pulsar.createOutputPort("MIDI Output0");
//		pulsar.createOutputPort("MIDI Output1");
//		pulsar.createInputPort("MIDI Input0");
//		pulsar.createInputPort("MIDI Input1");
//		pulsar.connectPort( "Metro:MIDI Output0", "hydrogen-midi:RX" );

		// metro.connectPort( "a2j:Xkey37 [20] (capture): Xkey37 MIDI 1", "Metro:MIDI Input0" );
	}

	static void pulsarError( String msg, Throwable e ) {
        Logger.getLogger(Pulsar.class.getName()).log(Level.SEVERE, null, e);
	}

	static final int BORDER = 10;
	
	Runnable mainProcedure = null;
	public void setMainProcedure( Runnable mainAction ) {
		this.mainProcedure = mainAction;
		reset();
	}
	public Runnable getMainProcedure() {
		return mainProcedure;
	}
	
	/**
	 * Stores the file path which modified the related-file-list. This path is used
	 * to resolve a relative path when a relative path is given to
	 * {@link Pulsar#loadScheme(File) } method.
	 */
	File relatedFileParent=null;
	
	final List<File> relatedFiles = new ArrayList<>();
	public List<File> getRelatedFiles() {
		return Collections.unmodifiableList( relatedFiles );
	}
	public void setRelatedFiles( List<File> fileList ) {
		this.relatedFiles.clear();
		this.relatedFiles.addAll( fileList );
		this.relatedFileParent = currentFile;

		if ( this.cb_relatedFiles != null ) {
			this.cb_relatedFiles.removeAllItems();
			for ( File f : this.relatedFiles ) {
				this.cb_relatedFiles.addItem(f.getPath());
			}
		}
	}
	/*
	 * This method is not used now.
	 */
	public void addRelatedFile( File f) {
		this.relatedFiles.add( f );
		if ( this.cb_relatedFiles != null ) {
			this.cb_relatedFiles.addItem( f.getPath() );
		}
	}

    /**
	 * {@link Pulsar#reset()} method resets the state of the object and calls main
	 * procedure to back to the state of beginning of the project.
	 */
    public void reset() { 
    	System.err.println( "===reset" );
    	setPlaying(false);
    	if ( mainProcedure != null )
    		mainProcedure.run();
		clearSequences();
    }
    public void cue() {
    	System.err.println( "===cue" );
    }
    
    /*
     * This method clears main procedure and state.
     */
    public void clear() { 
    	System.err.println( "===clear" );
    	this.setPlaying(false);
    	this.mainProcedure = null;
		clearSequences();
    }

	public File getConfigDir() {
		final File configDir = new File( System.getProperty("user.home"), ".pulsar" );
		if ( ! configDir.isDirectory() ) {
			if (! configDir.mkdir() ) {
				System.err.println( "WARNING : Failed to create the config directory." );
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
				System.err.println( "WARNING : Failed to create the main config file." );
				e.printStackTrace();
			}
		}
		return configFile;
	}

	public File getHistoryFile() {
		// Configuration Directory
		final File historyFile = new File( getConfigDir(), "history.txt" );
		if ( ! historyFile.isFile() ) {
			try {
				historyFile.createNewFile();
			} catch (IOException e) {
				System.err.println( "WARNING : Failed to create the history file." );
				e.printStackTrace();
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
			pulsarError( "" , e1 );
		} catch (IOException e1) {
			pulsarError( "" , e1 );
		} finally {
			if ( in != null )
				try {
					in.close();
				} catch (IOException e1) {
					pulsarError( "" , e1 );
				}
		}
	}
	
	private static final int NOT_DEFINED = -1;

	final File configFile = getConfigFile();
	long lastModifiedOfConfigFile=NOT_DEFINED;

	/**
	 * This stores the File object that point to the current opening scheme file.
	 * Pulsar's process of opening file is depend on watchdog timer. The watchdog
	 * timer frequently check the file that the {@link Pulsar#currentFile} points if
	 * its timestamp value is modified. When it detect timestamp modification, the
	 * watchdog timer invoke {@link Pulsar#loadScheme(File)} method to load the
	 * script.
	 */
	File currentFile=null;
	
	/**
	 * This path is used whenever a relative path is given to the
	 * {@link Pulsar#loadScheme(File)} method.
	 */
	File currentParentFile=null;

	/**
	 * This field stores the last value of timestamp of the {@link Pulsar#currentFile }
	 */
	long lastModifiedOfCurrentFile=NOT_DEFINED;
	
	void setCurrentFile( File currentParentFile, File currentFile ) {
		this.currentFile = currentFile;
		this.currentParentFile = currentParentFile;
		this.lastModifiedOfCurrentFile = NOT_DEFINED;
	}
	
	// a watchdog Timer
	{
		Timer timer = new Timer(1000, new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				lastModifiedOfConfigFile = checkLastModified( configFile, lastModifiedOfConfigFile );
				lastModifiedOfCurrentFile = checkLastModified( currentFile, lastModifiedOfCurrentFile );
			}

			long checkLastModified( File file, long lastModified ) {
				if ( file == null ) 
					return NOT_DEFINED; // lastModified;
				
				long newLastModified = file.lastModified();
				if ( newLastModified != lastModified ) {
					System.err.println( "Detected that the file was modified." );
					try {
						loadScheme( file );
					} catch (FileNotFoundException e) {
						pulsarError( "" , e );
					}
				}
				return newLastModified;
			}
		});
		timer.setInitialDelay(250);
		timer.start(); 
	}


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
				pulsarError( "" , e );
				throw new RuntimeException(e);
			}
		}
	}

	public void loadScheme(File file) throws FileNotFoundException {
		if ( ! file.isFile() ) {
			throw new FileNotFoundException( file.getPath() );
		}
		
		if ( ! file.isAbsolute() ) {
			file = new File( currentParentFile.getParentFile(), file.getPath() );
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

    void initScheme(Scheme scheme) {
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
    	scheme.getEnvironment().define( SimpleSymbol.make( "", "open?" ), null, new ProcedureN() {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				return running;
    		}
    	});

    	scheme.getEnvironment().define( SimpleSymbol.make( "", "open!" ), null, new ProcedureN() {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				for ( Object o : args ) {
					String name = SchemeUtils.toString( o );
					open( name );
				}
				return EmptyList.emptyList;
    		}
    	});
    	scheme.getEnvironment().define( SimpleSymbol.make( "", "close!" ), null, new ProcedureN() {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				close();
				return EmptyList.emptyList;
    		}
    	});
    	scheme.getEnvironment().define( SimpleSymbol.make( "", "output!" ), null, new ProcedureN() {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				for ( Object o : args ) {
					String name = SchemeUtils.toString( o );
					createOutputPort( name );
				}
				return EmptyList.emptyList;
    		}
    	});

    	scheme.getEnvironment().define( SimpleSymbol.make( "", "input!" ), null, new ProcedureN() {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				for ( Object o : args ) {
					String name = SchemeUtils.toString( o );
					createInputPort( name );
				}
				return EmptyList.emptyList;
    		}
    	});
    	scheme.getEnvironment().define( SimpleSymbol.make( "", "connect!" ), null, new ProcedureN() {
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
						pulsarError( "" , e );
					}
				}
				return EmptyList.emptyList;
    		}
    	});
    	

    	scheme.getEnvironment().define( SimpleSymbol.make( "", "set-main!" ), null, new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				System.err.println("set-main");
				if ( args.length == 1 ) {
					Procedure procedure = (Procedure)args[0];
					setMainProcedure( new SchemeProcedureRunnable( procedure ));
				} else {
					throw new RuntimeException( "invalid argument length" );
				}
    			return EmptyList.emptyList;
    		}
    	});
    	scheme.getEnvironment().define( SimpleSymbol.make( "", "set-playing!" ), null, new ProcedureN() {
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
    	scheme.getEnvironment().define( SimpleSymbol.make( "", "playing?" ), null, new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				return getPlaying();
    		}
    	});
    	scheme.getEnvironment().define( SimpleSymbol.make( "", "play!" ), null, new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				setPlaying( true ); 
    			return EmptyList.emptyList;
    		}
    	});
    	scheme.getEnvironment().define( SimpleSymbol.make( "", "stop!" ), null, new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				setPlaying( false ); 
    			return EmptyList.emptyList;
    		}
    	});
    	

    	scheme.getEnvironment().define( SimpleSymbol.make( "", "set-related-files!" ), null, new ProcedureN() {
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

    	scheme.getEnvironment().define( SimpleSymbol.make( "", "get-related-files" ), null, new ProcedureN() {
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

    	scheme.getEnvironment().define( SimpleSymbol.make( "", "cue!" ), null, new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				cue();
    			return EmptyList.emptyList;
    		}
    	});
    	scheme.getEnvironment().define( SimpleSymbol.make( "", "reset!" ), null, new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				reset();
    			return EmptyList.emptyList;
    		}
    	});
    	scheme.getEnvironment().define( SimpleSymbol.make( "", "clear!" ), null, new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				clear();
    			return EmptyList.emptyList;
    		}
    	});


//    	scheme.getEnvironment().define( SimpleSymbol.make( "", "put-seq!" ), null, new ProcedureN() {
//			@Override
//    		public Object applyN(Object[] args) throws Throwable {
//				if ( args.length == 2 ) {
//					String name = SchemeUtils.toString( args[0] );
//					Procedure procedure = (Procedure) args[1];
//					SchemePulsarLogic logic = new SchemePulsarLogic(procedure);
//		
//					putLogic(name, logic);
//					
//					return EmptyList.emptyList;
//				} else {
//					throw new RuntimeException( "Invalid parameter. usage : (-seq [name] [lambda] ) " );
//				}
//			}
//    	});

    	scheme.getEnvironment().define( SimpleSymbol.make( "", "put-seq!" ), null, new ProcedureN() {
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
   	    	
    	scheme.getEnvironment().define( SimpleSymbol.make( "", "remove-seq!" ), null, new ProcedureN() {
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
    	scheme.getEnvironment().define( SimpleSymbol.make( "", "clear-seq!" ), null, new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				clearSequences();
				return EmptyList.emptyList;
			}
    	});

    	scheme.getEnvironment().define( SimpleSymbol.make( "", "has-seq?" ), null, new ProcedureN() {
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


  	
    	// ???
    	scheme.getEnvironment().define( SimpleSymbol.make( "", "gui-get-pane" ), null, new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				System.err.println("gui-get-pane");
    			return userPane;
    		}
    	});
 
    	// ???
    	scheme.getEnvironment().define( SimpleSymbol.make( "", "gui-get-frame" ), null, new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				System.err.println("gui-get-frame");
    			return frame;
    		}
    	});

    	scheme.getEnvironment().define( SimpleSymbol.make( "", "gui-clear!" ), null, new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				guiClear();
    			return EmptyList.emptyList;
    		}

    	});
    	
    	


    	scheme.getEnvironment().define( SimpleSymbol.make( "", "new-button" ), null, new ProcedureN() {
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
										pulsarError( "" , e1 );
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
    	
    	scheme.getEnvironment().define( SimpleSymbol.make( "", "gui-add!" ), null, new ProcedureN() {
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
    	// TODO
    	scheme.getEnvironment().define( SimpleSymbol.make( "", "gui-newline!" ), null, new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				System.err.println("newline-gui");
				guiNewline();
    			return EmptyList.emptyList;
    		}
    	});

    	scheme.getEnvironment().define( SimpleSymbol.make( "", "gui-refresh" ), null, new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				System.err.println("refresh-gui");
				guiRefresh();
    			return EmptyList.emptyList;
    		}
    	});
    	scheme.getEnvironment().define( SimpleSymbol.make( "", "gui-gridbag-layout" ), null, new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				System.err.println( "gui-gridbag-layout" );
				guiGridBagLayout();
    			return EmptyList.emptyList;
    		}
    	});
    	scheme.getEnvironment().define( SimpleSymbol.make( "", "gui-spring-layout" ), null, new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				System.err.println( "gui-spring-layout" );
				guiSpringLayout();
    			return EmptyList.emptyList;
    		}
    	});
    	scheme.getEnvironment().define( SimpleSymbol.make( "", "gui-flow-layout" ), null, new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				System.err.println( "gui-flow-layout" );
				guiFlowLayout();
    			return EmptyList.emptyList;
    		}
    	});

    	scheme.getEnvironment().define( SimpleSymbol.make( "", "gui-put-constraint" ), null, new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				System.err.println("refresh-gui");
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
	JLabel tempoLabel = new JLabel("", SwingConstants.CENTER );

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
        
//        // https://stackoverflow.com/questions/17493207/java-how-to-let-jframe-to-resize-to-only-something-smaller-than-set-size
//        {
//			private static final long serialVersionUID = 1L;
//
//			@Override
//            public void paint(Graphics g) {
//                Dimension d = getSize();
//                Dimension m = getMaximumSize();
//                boolean resize = d.width > m.width || d.height > m.height;
//                d.width = Math.min(m.width, d.width);
//                d.height = Math.min(m.height, d.height);
//
//                if (resize) {
//                    Point p = getLocation();
//                    setVisible(false);
//                    setSize(d);
//                    setLocation(p);
//                    setVisible(true);
//                }
//                super.paint(g);
//            }        	
//        };
        
        
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

//		SpringUtilities.makeCompactGrid(staticPane, 1, 3, 1, 1 , 1, 1 );
		
//		for ( int i=0; i<staticPane.getComponentCount(); i++ ) {
//			JComponent c = (JComponent) staticPane.getComponent(i);
//			c.setPreferredSize( null );
//			c.setMaximumSize(new Dimension(Integer.MAX_VALUE, Integer.MAX_VALUE));
//		}
		
		 staticPane.setBorder( BorderFactory.createEmptyBorder(20,20,20,20) );
		
		
		{
//			staticPane.add( tempoLabel, BorderLayout.PAGE_START );
		}
		((JComponent)rootPane).setBorder( BorderFactory.createEmptyBorder() );
		
		
        //Display the window.
//		frame.setPreferredSize( new Dimension( 400, 400));
//		frame.setMaximumSize(new Dimension(300, Integer.MAX_VALUE ) );
//		userPane.setPreferredSize( new Dimension(400, 0 ) );
//		rootPane.setMaximumSize( new Dimension(400, 1000000 ) );
		
//		frame.addComponentListener( new ComponentAdapter() {
//			@Override
//			public void componentResized(ComponentEvent e) {
//				Component c = e.getComponent();
//				if ( 400 < c.getWidth() ) {
//					c.setSize( new Dimension( 400 , c.getHeight() ));
//					System.out.println( e.getClass() );
//				}
//			}
//		});

		// frame.setMaximizedBounds(new Rectangle(0, 0, 400, 1000));
		frame.pack();
		frame.setSize( 600, 500 );
		frame.setVisible(true);
    }
     

	public JPanel createFilePanel() {
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
					setCurrentFile( relatedFileParent, file );
				}
			}
		});
		
		panel.add( execButton , BorderLayout.LINE_START );

		cb_relatedFiles.addPopupMenuListener( new PopupMenuListener() {
			@Override
			public void popupMenuWillBecomeVisible(PopupMenuEvent e) {
				System.out.println("popupMenuWillBecomeVisible()");
				// readHistoryFile(comboBox);
			}
			
			@Override
			public void popupMenuWillBecomeInvisible(PopupMenuEvent e) {
				System.out.println( "popupMenuWillBecomeInvisible()");
			}
			
			@Override
			public void popupMenuCanceled(PopupMenuEvent e) {
				System.out.println(".popupMenuCanceled()");
			}
		});
		
		cb_relatedFiles.addItemListener(new ItemListener() {
			@Override
			public void itemStateChanged(ItemEvent e) {
				System.out.println("Pulsar.createFilePanel().new ItemListener() {...}.itemStateChanged()");
				if ( e.getStateChange() == ItemEvent.SELECTED ) {

//					if ( ! isComboBoxUpdating )  
//						SwingUtilities.invokeLater( new Runnable() {
//							@Override
//							public void run() {
//								try {
//									isComboBoxUpdating = true;
//									int i = comboBox.getSelectedIndex();
//									String item = comboBox.getItemAt(i);
//									comboBox.removeItemAt(i);
//									comboBox.insertItemAt(item, 0);
//									comboBox.setSelectedIndex(0);
//								} finally {
//									isComboBoxUpdating = false;
//								}
//
//							}
//						});
				}
			}
		});
		
//		readHistoryFile(cb_relatedFiles);

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
					setCurrentFile( null, fc.getSelectedFile() );
				}
			}
		});

		tf_currentFile = new JTextField();
		mainFilePanel.add( tf_currentFile, BorderLayout.CENTER );
		tf_currentFile.setEditable(false);
		

		return panel;
	}
    
	public JButton createStartStopButton() {
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
	public JButton createResetButton() {
		JButton b = new JButton( "RESET" );
		b.addActionListener( new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				reset();
			}
		});
		return b;
	}
    
	public JButton createCueButton() {
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
    
    public JButton createTempoTapButton() {
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
    			System.out.println(  current_diff );

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
    					System.err.println( String.format( "%.2f / %.2f = %.2f", onemin , avg , beatPerMinute  ) );
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
	