package org.jaudiolibs.test;

import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JPanel;

import nu.oka.metro.Metro;
import nu.oka.metro.MetroLogic;
import nu.oka.metro.MetroMasterLogic;
import nu.oka.metro.MetroMidiEventBuffer;

final class MetroLogicInputTest extends MetroMasterLogic.Default {
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

	@Override
	public boolean processBuffer( MetroMidiEventBuffer buf ) {
		// System.out.println("Metro.logic.new MetroLogic() {...}.initBuffer()" );

		buf.humanize( 0.0d, 3 );
		
		buf.noteShot( 0.0d  , 1 , 0, 57, 105 );
		buf.noteShot( 0.02d , 1 , 0, 74, 127 );
		buf.noteShot( 0.2d  , 1 , 0, 73, 100 );
		buf.noteShot( 0.4d  , 1 , 0, 73, 100 );
		buf.noteShot( 0.6d  , 1 , 0, 73, 100 );
		buf.noteShot( 0.8d  , 1 , 0, 73, 100 );
		buf.length(     1.00d );

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
		panel.add(bld.makeButton("Button 3", "Button 3"));
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