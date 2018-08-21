package ats.pulsar;

import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.LayoutManager;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SpringLayout;
import javax.swing.SwingConstants;
import javax.swing.Timer;

import org.jaudiolibs.jnajack.JackException;

import ats.metro.Metro;
import ats.metro.MetroNoteEventBufferSequence.SyncType;
import ats.pulsar.lib.FlawLayout;
import ats.pulsar.lib.LayoutUtils;
import ats.pulsar.lib.SpringLayoutUtil;
import gnu.lists.EmptyList;
import gnu.lists.IString;
import gnu.mapping.Environment;
import gnu.mapping.Procedure;
import gnu.mapping.ProcedureN;
import gnu.mapping.SimpleSymbol;
import kawa.standard.Scheme;

public final class Pulsar extends Metro {
	Pulsar parent = this;
	public Pulsar() throws JackException {
		javax.swing.SwingUtilities.invokeLater( new Runnable() {
			public void run() {
				createAndShowGUI();
			}
		});
	}
	
	public void guiInit() {
		userPane.removeAll();
	}
	public void guiSpringLayout() {
		userLayout = new SpringLayout();
		userPane.setLayout(userLayout);
	}
	public void guiFlowLayout() {
		userLayout = new FlawLayout();
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
		JPanel pane = userPane;
		pane.add( c );
		pane.revalidate();
		pane.repaint();
	}
	public void newlineGui() {
		userPane.revalidate();
		userPane.repaint();
	}

	public void endlineGui() {
		// userPane.add( Box.createVerticalStrut(500 ) );
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
	            Logger.getLogger(Metro.class.getName()).log(Level.SEVERE, null, e);
				throw new RuntimeException(e);
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
    	scheme.getEnvironment().define( SimpleSymbol.make( "", "set-main!" ), null, new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				System.err.println("set-main");
				if ( args.length == 1 ) {
					Procedure procedure = (Procedure)args[0];
					setMain( new SchemeProcedureRunnable( procedure ));
				} else {
					throw new RuntimeException( "invalid argument length" );
				}
    			return EmptyList.emptyList;
    		}
    	});
    	scheme.getEnvironment().define( SimpleSymbol.make( "", "playing!" ), null, new ProcedureN() {
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
					
					SchemePulsarLogic logic = new SchemePulsarLogic(procedure);
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

    	scheme.getEnvironment().define( SimpleSymbol.make( "", "gui-init!" ), null, new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				guiInit();
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
							            Logger.getLogger(Metro.class.getName()).log(Level.SEVERE, null, e1);
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
    	// TODO
    	scheme.getEnvironment().define( SimpleSymbol.make( "", "endline-gui" ), null, new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				System.err.println("newline-gui");
				endlineGui();
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

	//Create the "cards".
    JFrame frame = null;
    Container rootPane = null; 
	JPanel staticPane = new JPanel();

	LayoutManager userLayout = null; 
	JPanel userPane = new JPanel( new FlawLayout() );
	JLabel tempoLabel = new JLabel("", SwingConstants.CENTER );
    
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
		staticPane.setLayout( new BorderLayout(10,10) );
        frame.getContentPane().add ( rootPane );

		// Tempo Button
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
		
		// Config Directory
		final File configDir = new File( System.getProperty("user.home"), ".pulsar" );
		if ( ! configDir.isDirectory() ) {
			if (! configDir.mkdir() ) {
				System.err.println( "WARNING : Failed to create the config directory." );
			}
		}
		final File configFile = new File( configDir, "pulsar.scm" );
		if ( ! configFile.isFile() ) {
			try {
				configDir.createNewFile();
			} catch (IOException e) {
				System.err.println( "WARNING : Failed to create the main config file." );
				e.printStackTrace();
			}
		}
						
		// a watchdog Timer
		{
			Timer timer = new Timer(1000, new ActionListener() {
				private long last=-1;
				@Override
				public void actionPerformed(ActionEvent e) {
					long current = configFile.lastModified();
					if ( current != last ) {
						System.err.println( "Detected that the config file was modified." );
						try {
							String text = new String(Files.readAllBytes( Paths.get( configFile.toURI() ) ), StandardCharsets.UTF_8);
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
					this.last= current;
				}
			});
			timer.setInitialDelay(250);
			timer.start(); 
		}
		
		{
			staticPane.add( tempoLabel, BorderLayout.PAGE_START );
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
    
	Runnable main = null;
	public void setMain( Runnable mainAction ) {
		this.main = mainAction;
		reset();
	}
	public Runnable getMain() {
		return main;
	}

    public void reset() { 
    	System.err.println( "===reset" );
    	parent.setPlaying(false);
		main.run();
		parent.resetSequences();
    }
    public void cue() {
    	System.err.println( "===cue" );
    }
    
	public JButton createStartStopButton() {
		JButton b = new JButton( "<</II" );
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
    	Pulsar logic = this;
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
    					logic.setBeatsPerMinute( (long) beatPerMinute );
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
    
  
	public static void main(String[] args) throws JackException, InterruptedException {
        Pulsar metro = new Pulsar( );
        metro.start( "Metro" );

		metro.createOutputPort("MIDI Output0");
		metro.createOutputPort("MIDI Output1");
		metro.createInputPort("MIDI Input0");
		metro.createInputPort("MIDI Input1");
		metro.connectPort( "Metro:MIDI Output0", "hydrogen-midi:RX" );
		// metro.connectPort( "a2j:Xkey37 [20] (capture): Xkey37 MIDI 1", "Metro:MIDI Input0" );
		
	}
}
	