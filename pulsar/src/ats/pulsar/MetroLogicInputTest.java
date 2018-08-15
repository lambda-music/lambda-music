package ats.pulsar;

import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

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
import ats.metro.MetroLogic;
import ats.metro.MetroMasterLogic;
import ats.metro.MetroMidiEvent;
import ats.metro.MetroNoteEventBuffer;
import gnu.lists.AbstractSequence;
import gnu.lists.EmptyList;
import gnu.lists.IString;
import gnu.lists.Pair;
import gnu.mapping.Procedure;
import gnu.mapping.ProcedureN;
import gnu.mapping.SimpleSymbol;
import gnu.math.IntNum;
import kawa.standard.Scheme;

final class MetroLogicInputTest extends MetroMasterLogic.Default {
	public MetroLogicInputTest() {
		super();
	}
	@Override
	public String clientName() {
		return "Metro";
	}

	@Override
	public Set<Entry<String, String>> optionalConnection() {
		Map<String,String> outputPortList = new LinkedHashMap<String,String>();
		// outputPortList.put( "MIDI Output0", null );
		outputPortList.put( "Metro:MIDI Output1", "hydrogen-midi:RX" );
		outputPortList.put("a2j:Xkey37 [20] (capture): Xkey37 MIDI 1", "Metro:MIDI Input0" );
		
		// outputPortList.put( "Metro:MIDI Output1", "system:midi_playback_1" );

		return outputPortList.entrySet();
	}

	@Override
	public List<String> outputPortNameList() {
		return Arrays.asList( "MIDI Output0", "MIDI Output1" );
	}
	@Override
	public List<String> inputPortNameList() {
		return Arrays.asList( "MIDI Input0", "MIDI Input1" );
	}

	private boolean flag;
	private void notifyFlag() {
		this.flag = true;
	}

	/*
	 * (list
	 *     (cons 1.0  (lambda() ... ))
	 *     (cons 2.0  (lambda() ... ))
	 *     (cons 2.5  (lambda() ... )))
	 */
	static ArrayList<Pair> parseListOfPairs( Pair pair ) {
		ArrayList<Pair> pairs = new ArrayList<Pair>();
		for ( Object pp : pair ) {
			pairs.add( (Pair)pp ); 
		}
		return pairs;
	}

	static class SchemePulsable implements Pulsable {
		final double bars;
		final Procedure procedure;
		public SchemePulsable(double bars, Procedure procedure) {
			this.bars = bars;
			this.procedure = procedure;
		}

		@Override
		public void pulse(MetroNoteEventBuffer buf) {
			AbstractSequence<Object> pattern ;
			try {
				pattern = (AbstractSequence<Object>) procedure.applyN( new Object[] {} );
			} catch (Throwable e) {
				e.printStackTrace();
				pattern = null;
			}
			
			if ( pattern != null ) {
				for ( Iterator<Object> i = pattern.iterator(); i.hasNext(); ) {
					Pair ep = (Pair)i.next();
					Map<String,Object> map = SchemeUtils.list2map(ep, (Integer idx)->{
//						double offset, int outputPortNo, int channel, int note, int velocity 
						switch ( idx ) {
							case 0:
								return "offset";
							case 1:
								return "portno";
							case 2:
								return "channel";
							case 3:
								return "note";
							case 4:
								return "velocity";
							default :
								return Integer.toString( idx );
						}
					});
					double offset    = map.containsKey( "offset"   ) ? SchemeUtils.toDouble( map.get("offset"    ) ) : 0.0d;  
					int outputPortNo = map.containsKey( "portno"   ) ? SchemeUtils.toInteger( map.get("portno"   ) ) : 1;
					int channel      = map.containsKey( "channel"  ) ? SchemeUtils.toInteger( map.get("channel"  ) ) : 0; 
					int note         = map.containsKey( "note"     ) ? SchemeUtils.toInteger( map.get("note"     ) ) : 63;  
					int velocity     = map.containsKey( "velocity" ) ? SchemeUtils.toInteger( map.get("velocity" ) ) : 63;
				
//					System.out.println( offset );
//					System.out.println( note );
					
					buf.noteShot(offset, outputPortNo, channel, note, velocity);
				}
			}
			buf.setLength( this.bars );
			
			// buf.noteShot(0, 1, 0, 73, 100 );
		}

		@Override
		public double getBars() {
			return bars;
		}
	}
	
	static class SchemePulsableBuilder implements PulsableBuilder {
		String name;
		String description;
		List<SchemePulsable> pulsableList;
		@Override
		public String getName() {
			return name;
		}

		public String getDescription() {
			return description;
		}

		public List<SchemePulsable> getPulsableList() {
			return pulsableList;
		}

		public SchemePulsableBuilder( String name, String description, List<Pair> pairs ) {
			super();
			this.name = name;
			this.description = description;
			this.pulsableList = new ArrayList<>();
			for ( int i=0; i< pairs.size();i++  ) {
				Pair p = pairs.get(i);
				pulsableList.add( new SchemePulsable( SchemeUtils.toDouble( p.getCar() ) , (Procedure) p.getCdr() ) );
			}
		}

		@Override
		public List<Pulsable> create() {
			return new ArrayList<>( this.pulsableList );
		}
	}
	
	static class SamplePulsableBuilder implements PulsableBuilder {
		String name;
		@Override
		public String getName() {
			return "default";
		}
		
		@Override
		public List<Pulsable> create() {
			ArrayList<Pulsable> result = new ArrayList<Pulsable>();
			
			result = new ArrayList<Pulsable>();
			{
				JavaPulse[][] arr = {
						{ new JavaPulse(73) },
						{},
						{},
						{ new JavaPulse(73) },
						{},
						{ new JavaPulse(true, 73,90) },
				};
				result.add( new JavaPulseList( arr, 7,2, 3 ) );
			}
			{
				JavaPulse[][] arr = {
					{ new JavaPulse(63,80) },
				};
				result.add( new JavaPulseList( arr, 4 ,2, 0 ) );
			}
			{
				JavaPulse[][] arr = {
					{ new JavaPulse(57,80) },
				};
				result.add( new JavaPulseList( arr, 1 ,2, 0 ) );
			}
			
			return result;
		}
	}


	List<Pulsable> pulsableList = new ArrayList<Pulsable>(); 
	{
		System.err.println("set-current-pulsable (from init)" );
		setCurrentPulsable( new SamplePulsableBuilder() );
	}
	
	public void setCurrentPulsable( PulsableBuilder pulsableBuilder ) {
		System.err.println( "set current pulsable "  + pulsableBuilder.getName() );
		pulsableList.clear();
		pulsableList.addAll( pulsableBuilder.create() );
		
//		XXX
//		if ( getParent() != null )
//			getParent().clearSequences();
	}
	
	

	List<PulsableBuilder> pulsableBuilderList = new ArrayList<PulsableBuilder>();
	public void clearPulseableBuilderList() {
		pulsableBuilderList.clear();
		userPane.removeAll();
		
		initGui();
		addPulsableBuilderList( new SamplePulsableBuilder() );
		refreshGui();
	}
	
	public JButton addPulsableBuilderList( PulsableBuilder pulsableBuilder ) {
		pulsableBuilderList.add( pulsableBuilder );

		JButton b = new JButton( pulsableBuilder.getName() );
		b.addActionListener( new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				System.err.println( "Set current pulsable object to " + pulsableBuilder.getName()  );
				setCurrentPulsable( pulsableBuilder );
			}
		});
		userPane.add( b );
		userPane.revalidate();
		userPane.repaint();
		return b;
	}
	
	int constraintNextCount= 0;
	public void putConstraintNext() {
		int count = userPane.getComponentCount();
		if ( 2 < count ) {
			System.out.println("HELLO");
			userLayout.putConstraint( 
					SpringLayout.WEST,
					userPane.getComponent( count -1 ),
					3,
					SpringLayout.EAST,
					userPane.getComponent( count -2 )
					);
			constraintNextCount = constraintNextCount +1;
			System.err.println( userPane.getComponent( count -1 ).getName() );
			System.err.println( userPane.getComponent( count -2 ).getName() );
			System.err.println( "=============" );
		}
	}
	public void putConstraintNewLine() {
		int count = userPane.getComponentCount();
		if ( 2 < count ) {
			System.out.println("NEWLINE");
			userLayout.putConstraint( 
					SpringLayout.NORTH,
					userPane.getComponent( count -1 ),
					3,
					SpringLayout.SOUTH,
					userPane.getComponent( count - constraintNextCount )
					);
			constraintNextCount = 0;
		}
	}

	public void initGui() {
	}

	public void initGuiBak() {
		JButton b01 = new JButton( "HELLO1" );
		JButton b02 = new JButton( "HELLO2" );
		JButton b03 = new JButton( "HELLO3" );
//		JButton b04 = new JButton( "HELLO4" );
//		JButton b05 = new JButton( "HELLO5" );
		userPane.add( b01 );
		userPane.add( b02 );
		userPane.add( b03 );
//		userPane.add( b04 );
//		userPane.add( b05 );
		
		SpringLayoutUtil util = new SpringLayoutUtil(userLayout, userPane);
//
		int gap = 5;

		util.putConstraint( "N", b01, gap, "N", userPane );
//		util.putConstraint( "E", b01, gap, "W", b02 );
		util.putConstraint( "W", b01, gap, "W", userPane );
		util.putConstraint( "S", b01, -gap, "S", userPane );
		
		util.putConstraint( "N", b02, gap, "N", userPane );
//		helper.putConstraint( "E", b02, gap, "W", b03 );
		util.putConstraint( "W", b02, gap, "E", b01 );
		util.putConstraint( "S", b02, -gap, "S", userPane );

		util.putConstraint( "N", b03, gap, "N", userPane );
		util.putConstraint( "E", b03, - gap, "E", userPane );
		util.putConstraint( "W", b03, gap, "E", b02 );
		util.putConstraint( "S", b03, -gap, "S", userPane );

//		userPane.add( new JButton("HELLO3") );
//		putConstraint( "East",  -1,   3, "West",   0 );
//		putConstraint( "North",9999,  3, "North",  0 );
	}
	
	public void refreshGui() {
		userPane.revalidate();
		userPane.repaint();
        frame.pack();
	}

	
	@Override
	public void processInputMidiBuffer(List<MetroMidiEvent> in, List<MetroMidiEvent> out) {
		out.addAll( in );
		System.err.println( "in.size()" + in.size() );
		System.err.println( "out.size()" + out.size() );
	}


	@Override
	public boolean processOutputNoteBuffer( MetroNoteEventBuffer buf ) {
		// System.out.println("Metro.logic.new MetroLogic() {...}.initBuffer()" );

		buf.humanize( 0.0d, 3 );

		double maxBars = 0.0d;
		for ( Pulsable pulsable : pulsableList ) {
			pulsable.pulse( buf );
			if ( maxBars < pulsable.getBars() )
				maxBars = pulsable.getBars();
		}
		
		// System.err.println( "maxBars" +  maxBars );
		buf.length( maxBars );

		
//		buf.noteShot( 0.0d  , 1 , 0, 57, 105 );
//		buf.noteShot( 0.02d , 1 , 0, 74, 127 );
////		buf.noteShot( 0.00d , 1 , 0, 74, 127 );
//		buf.noteShot( 0.2d  , 1 , 0, 73, 100 );
//		buf.noteShot( 0.4d  , 1 , 0, 73, 100 );
//		buf.noteShot( 0.6d  , 1 , 0, 73, 100 );
//		buf.noteShot( 0.8d  , 1 , 0, 73, 100 );
//		buf.length(     1.00d );

		if ( flag ) {
			handle.spawn( 0.1d, new MetroLogic.Default() {
				int cnt = 2;
				@Override
				public boolean processOutputNoteBuffer(MetroNoteEventBuffer buf) {
					//				buf.noteShot( 0.5d  , 1 , 0, 57, 127 );

					buf.noteShot( 0.0d  , 1 , 0, 63, 127 );
					buf.noteShot( 0.2d  , 1 , 0, 63, 80 );
					buf.noteShot( 0.4d  , 1 , 0, 63, 80 );
					buf.noteShot( 0.6d  , 1 , 0, 63, 80 );
					buf.noteShot( 0.8d  , 1 , 0, 63, 80 );
					buf.length(1.0d);
					return 0<cnt--;
				}
				@Override
				public void processInputMidiBuffer(List<MetroMidiEvent> in, List<MetroMidiEvent> out) {
				}
			});
			flag = false;
		}
		
		return true;
	}
	
    @Override
    public void initialize() {
		javax.swing.SwingUtilities.invokeLater( new Runnable() {
			public void run() {
				createAndShowGUI();
			}
		});
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
    	
    	scheme.getEnvironment().define( SimpleSymbol.make( "", "sum-aa" ), null, new ProcedureN() {
    		@Override
    		public Object applyN(Object[] args) throws Throwable {
    			int sum=0;
    			for ( int i=0; i<args.length; i++ ) {
    				if ( args[i] instanceof IntNum  ) {
    					sum += IntNum.intValue( args[i] );
    				}
    			}
    			return sum;
    		}
    	});
    	scheme.getEnvironment().define( SimpleSymbol.make( "", "set-current-pattern" ), null, new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				if ( args.length == 1 ) {
					String name = ((IString) args[0]).toString();
					for ( PulsableBuilder b : pulsableBuilderList ) {
						if ( b.getName().equals( name ) ) {
							System.err.println("set-current-pulsable (from scheme)" );
							setCurrentPulsable( b );
							break;
						}
					}
					
				} else {
					// Set the passed object as a builder function directly.
					String name = ((IString) args[0]).toString();
					String description = ((IString) args[1]).toString();
					ArrayList<Pair> pairs = parseListOfPairs( (Pair) args[2] );
					
					SchemePulsableBuilder pulsableBuilder = new SchemePulsableBuilder( name,description, pairs );
					setCurrentPulsable( pulsableBuilder );
				}
    			return EmptyList.emptyList;
    		}
    	});

    	scheme.getEnvironment().define( SimpleSymbol.make( "", "clear-pattern" ), null, new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				clearPulseableBuilderList();
    			return EmptyList.emptyList;
    		}

    	});

    	scheme.getEnvironment().define( SimpleSymbol.make( "", "add-pattern" ), null, new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				// System.err.println("add-pattern");
    			String name = ((IString) args[0]).toString();
				String description = ((IString) args[1]).toString();
				ArrayList<Pair> pairs = parseListOfPairs( (Pair) args[2] );
				SchemePulsableBuilder pulsableBuilder = new SchemePulsableBuilder( name,description, pairs );

				
    			return addPulsableBuilderList( pulsableBuilder );
    		}

    	});
    	scheme.getEnvironment().define( SimpleSymbol.make( "", "get-gui" ), null, new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				System.err.println("get-gui");
    			return userPane;
    		}
    	});
    	scheme.getEnvironment().define( SimpleSymbol.make( "", "get-frame" ), null, new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				System.err.println("get-frame");
				// System.err.println( frame );
    			return frame;
    		}
    	});
    	scheme.getEnvironment().define( SimpleSymbol.make( "", "add-button" ), null, new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				if ( args.length == 3 ) {
					ProcedureN p = (ProcedureN) args[2];
					String caption = ((IString) args[0]).toString();
					Object userHandle = args[1];
					JButton button = new JButton( caption );
					button.addActionListener(new ActionListener() {
						@Override
						public void actionPerformed(ActionEvent e) {
							try {
								p.applyN( new Object[] { userHandle } );
							} catch (Throwable e1) {
								e1.printStackTrace();
							}
						}
					});
					userPane.add( button );
					return button;
				} else {
					throw new RuntimeException( "add-button has two parameters( caption user-handle lambda )." );
				}
    		}
    	});
    	scheme.getEnvironment().define( SimpleSymbol.make( "", "refresh-gui" ), null, new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				System.err.println("refresh-gui");
				refreshGui();
    			return EmptyList.emptyList;
    		}
    	});
    	scheme.getEnvironment().define( SimpleSymbol.make( "", "put-constraint" ), null, new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				System.err.println("refresh-gui");
				if ( args.length == 5 ) {
					new SpringLayoutUtil(userLayout, userPane).putConstraint( args[0],args[1],args[2],args[3],args[4] );
				} else if ( args.length == 4 ) {
					new SpringLayoutUtil(userLayout, userPane).putConstraint( args[0],args[1],5, args[2],args[3] );
				} else {
					throw new RuntimeException( "put-constraint has five parameters( constraint1 component1 pad constraint2 component2  )." );
				}
				refreshGui();
    			return EmptyList.emptyList;
    		}
    	});
    }

	//Create the "cards".
    JFrame frame = null;
    Container rootPane = null; 
	JPanel staticPane = new JPanel( new FlowLayout( FlowLayout.CENTER ) );
	SpringLayout userLayout = new SpringLayout();
	JPanel userPane = new JPanel( userLayout );
	JLabel tempoLabel = new JLabel("", SwingConstants.CENTER );
    
    private void createAndShowGUI() {
    	MetroLogicInputTest logic = this;
        //Create and set up the window.
        frame = new JFrame("Metro Logic");
        
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
		staticPane.setLayout( new BorderLayout() );
        frame.getContentPane().add ( rootPane );

		// Tempo Button
		staticPane.add( createTempoTapButton(), BorderLayout.CENTER );
		staticPane.setBorder( BorderFactory.createEmptyBorder(20,20,20,20) );
		
		// a watchdog Timer
		{
			final File configFile = new File( System.getProperty("user.home"), ".pulsar" );
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
		frame.setSize(400, 600 );
		frame.pack();
		frame.setVisible(true);
    }
    
    public JButton createTempoTapButton() {
    	MetroLogicInputTest logic = this;
    	JButton tempoTapButton = new JButton( "Tempo" );
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
    					logic.getParent().setBeatsPerMinute( (long) beatPerMinute );
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
    
  
	public static void main(String[] args) {
        MetroLogicInputTest logic = new MetroLogicInputTest();
		Metro.startClient( logic );
	}
}
