package org.jaudiolibs.examples;

import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JPanel;

import org.jaudiolibs.jnajack.JackException;

import ats.metro.Metro;
import ats.metro.MetroLogic;
import ats.metro.MetroMidiEvent;
import ats.metro.MetroNoteEventBuffer;

final class MetroLogicFiveTimes extends MetroLogic.Default {
	public MetroLogicFiveTimes() {
		super();
	}

	@SuppressWarnings("unused")
	private int cnt=1;

	private boolean flag;
	private void notifyFlag() {
		this.flag = true;
	}
	
	@Override
	public void processInputMidiBuffer(List<MetroMidiEvent> in, List<MetroMidiEvent> out) {
	}

	@Override
	public boolean processOutputNoteBuffer( MetroNoteEventBuffer buf) {
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
			handle.spawn( XXX, 0.1d, new MetroLogic.Default() {
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
		
		cnt++;
		return true;
	}
	
	

    private static void createAndShowGUI( MetroLogicFiveTimes logic ) {
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
    
  
	public static void main(String[] args) throws JackException {
        MetroLogicFiveTimes logic = new MetroLogicFiveTimes();
		Metro metro = Metro.startClient( "Metro", logic );

		metro.createOutputPort( "MIDI Output0" );
		metro.createOutputPort( "MIDI Output1" );
		metro.connectPort( "Metro:MIDI Output1", "hydrogen-midi:RX" );
		createAndShowGUI( logic );
		metro.setPlaying(true);
	}
}