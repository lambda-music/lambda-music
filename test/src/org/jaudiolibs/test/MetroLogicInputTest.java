package org.jaudiolibs.test;

import static org.jaudiolibs.test.MetroLogicInputTest.MetroPattern.create;

import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JPanel;

import org.jaudiolibs.jnajack.JackException;

import nu.oka.metro.Metro;
import nu.oka.metro.MetroLogic;
import nu.oka.metro.MetroMasterLogic;
import nu.oka.metro.MetroMidiEventBuffer;

final class MetroLogicInputTest extends MetroMasterLogic.Default {

	static final class Pulse {
		static final int DEFAULT_PORT = 1;
		public final boolean centerPulse;
		public final int port;
		public final int channel;
		public final int note;
		public final int velocity;
		
		public Pulse(boolean centerPulse, int port, int channel, int note, int velocity) {
			super();
			this.centerPulse= centerPulse;
			this.port = port;
			this.channel = channel;
			this.note = note;
			this.velocity = velocity;
		}
		public Pulse(int port, int channel, int note, int velocity) {
			super();
			this.centerPulse= false;
			this.port = port;
			this.channel = channel;
			this.note = note;
			this.velocity = velocity;
		}
		public Pulse(int channel, int note, int velocity) {
			super();
			this.centerPulse= false;
			this.port = DEFAULT_PORT;
			this.channel = channel;
			this.note = note;
			this.velocity = velocity;
		}
		public Pulse(int note, int velocity ) {
			super();
			this.centerPulse= false;
			this.port = DEFAULT_PORT;
			this.channel = 1;
			this.note = note;
			this.velocity = velocity;
		}
		public Pulse(int note ) {
			super();
			this.centerPulse= false;
			this.port = DEFAULT_PORT;
			this.channel = 0;
			this.note = note;
			this.velocity = 105;
		}
		public Pulse(boolean centerPulse, int channel, int note, int velocity) {
			super();
			this.centerPulse= centerPulse;
			this.port = DEFAULT_PORT;
			this.channel = channel;
			this.note = note;
			this.velocity = velocity;
		}
		public Pulse(boolean centerPulse, int note, int velocity ) {
			super();
			this.centerPulse= centerPulse;
			this.port = DEFAULT_PORT;
			this.channel = 1;
			this.note = note;
			this.velocity = velocity;
		}
		public Pulse(boolean centerPulse, int note ) {
			super();
			this.centerPulse= centerPulse;
			this.port = DEFAULT_PORT;
			this.channel = 0;
			this.note = note;
			this.velocity = 105;
		}
	}
	
	static abstract interface MetroPattern {
		static List<Set<Pulse>> create(Pulse[][] list ) {
			List<Set<Pulse>> result = new ArrayList<Set<Pulse>>();
			for ( Pulse[] set : list ) {
				 result.add( new HashSet<Pulse>( Arrays.asList( set )  ) );
			}
			return result;
		}
		List<Set<Pulse>> getTuplets();
		int getBeats();
		int getBars();
		static class Static implements MetroPattern {
			List<Set<Pulse>> tuplets;
			int beats;
			int bars;
			int centerBeat;
			public Static(Pulse[][] tuplets, int beats, int bars, int centerBeat ) {
				super();
				this.tuplets = create( tuplets );
				this.beats = beats;
				this.bars = bars;
				this.centerBeat = centerBeat;
			}

			public Static(List<Set<Pulse>> tuplets, int beats, int bars) {
				super();
				this.tuplets = tuplets;
				this.beats = beats;
				this.bars = bars;
			}
			public List<Set<Pulse>> getTuplets() {
				return tuplets;
			}
			public int getBeats() {
				return beats;
			}
			public int getBars() {
				return bars;
			}
			public int getCenterBeat() {
				return centerBeat;
			}
		}
	}
	
	public MetroLogicInputTest() {
		super();
	}

	private int cnt=1;

	@Override
	public String clientName() {
		return "Metro";
	}

	@Override
	public Set<Entry<String, String>> optionalConnection() {
		Map<String,String> outputPortList = new LinkedHashMap<String,String>();
		// outputPortList.put( "MIDI Output0", null );
		outputPortList.put( "Metro:MIDI Output1", "hydrogen-midi:RX" );
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
	
	List<MetroPattern> currentPatternList; 
	{
		currentPatternList = new ArrayList<MetroPattern>( );
//		currentPatternList.add( 
//				new MetroPattern.Static(
//						Arrays.asList(
//								new HashSet<MetroNote>( Arrays.asList( new MetroNote(57 ),new MetroNote(74 ) ) ),
//								new HashSet<MetroNote>( Arrays.asList( new MetroNote(73 ))),
//								new HashSet<MetroNote>( Arrays.asList( new MetroNote(73 ))),
//								new HashSet<MetroNote>( Arrays.asList( new MetroNote(73 ))),
//								new HashSet<MetroNote>( Arrays.asList( new MetroNote(73 ))),
//								new HashSet<MetroNote>( Arrays.asList( new MetroNote(73 ))),
//								new HashSet<MetroNote>( Arrays.asList( new MetroNote(73 )))
//								),
//						1,1	));

		currentPatternList.add( 
				new MetroPattern.Static(
						new Pulse[][] {
							{ new Pulse(73) },
							{},
							{},
							{ new Pulse(74) },
							{},
							{ new Pulse(true, 74) },
						}
						, 7,1, 0 ) );

		currentPatternList.add( 
				new MetroPattern.Static(
						new Pulse[][] {
							{ new Pulse(57) },
						}
						, 8 ,1 ,0 ) );
	}

	@Override
	public boolean processBuffer( MetroMidiEventBuffer buf ) {
		// System.out.println("Metro.logic.new MetroLogic() {...}.initBuffer()" );

		buf.humanize( 0.0d, 3 );
		
		for ( MetroPattern pattern : currentPatternList ) {
			List<Set<Pulse>> tuplets = pattern.getTuplets();
			int beats = pattern.getBeats();
			double bars = (double)pattern.getBars();
			
			double beatLength = (bars/(double)beats );
			double pulseLength = beatLength / tuplets.size();
			for ( int b = 0; b<beats; b ++ ) {
				double beatPos = beatLength * b;
				double pulseIndex = 0;
				for ( Set<Pulse> noteSet : tuplets ) {
					double pulsePos = ( pulseIndex * pulseLength + beatPos );
					for ( Pulse note : noteSet ) {
						buf.noteShot( pulsePos, note.port , note.channel, note.note, note.velocity );
					}
					pulseIndex ++;
				}
			}
		}
		buf.length(     1.00d );
		
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
				public boolean processBuffer(MetroMidiEventBuffer buf) {
					//				buf.noteShot( 0.5d  , 1 , 0, 57, 127 );

					buf.noteShot( 0.0d  , 1 , 0, 63, 127 );
					buf.noteShot( 0.2d  , 1 , 0, 63, 80 );
					buf.noteShot( 0.4d  , 1 , 0, 63, 80 );
					buf.noteShot( 0.6d  , 1 , 0, 63, 80 );
					buf.noteShot( 0.8d  , 1 , 0, 63, 80 );
					buf.length(1.0d);
					return 0<cnt--;
				}

			});
			flag = false;
		}
		
		cnt++;
		return true;
	}
	
    @Override
    public void initialize() {
		javax.swing.SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				createAndShowGUI( MetroLogicInputTest.this );
			}
		});
    }
	

    private static void createAndShowGUI( MetroLogicInputTest logic ) {
        //Create and set up the window.
        JFrame frame = new JFrame("Metro Logic");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
         
        //Create and set up the content pane.
        Container pane = frame.getContentPane();

        ActionListener listener = new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				switch ( e.getActionCommand() ) {
				case "5 Meter" :
					System.out.println("5 Meter");
					logic.notifyFlag();
					break;
				case "7 Meter" :
					System.out.println("7 Meter");
					break;
				case "Change Tempo" :
					try {
						logic.getParent().setBeatsPerMinute(300);
					} catch (JackException e1) {
						throw new RuntimeException(e1);
						// e1.printStackTrace();
					}
					break;
				default :
				}
			}
		};
		

		final class Builder {
			JButton makeButton( String caption, String actionCommand ) {
				JButton b = new JButton( caption );
				b.setActionCommand(actionCommand);
				b.addActionListener( listener );
				return b;
			}
		}
		Builder bld = new Builder();
        
		//Create the "cards".
		JPanel panel = new JPanel();
		
		panel.add(bld.makeButton("5", "5 Meter"));
		panel.add(bld.makeButton("7", "7 Meter"));
		panel.add(bld.makeButton("Change Tempo", "Change Tempo"));
		panel.add(bld.makeButton("Button 4", "Button 4"));
		panel.add(bld.makeButton("Button 5", "Button 5"));
		panel.add(bld.makeButton("Button 6", "Button 6"));
		
		
		pane.add( panel, BorderLayout.CENTER );
		
        //Display the window.
        frame.pack();
        frame.setSize(300, frame.getHeight());
        frame.setVisible(true);
    }
    
  
	public static void main(String[] args) {
        MetroLogicInputTest logic = new MetroLogicInputTest();
		Metro.startClient( logic );
	}
}