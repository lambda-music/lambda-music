package ats.pulsar.old;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import ats.metro.Metro;
import ats.metro.MetroLogic;
import ats.metro.MetroMidiEvent;
import ats.metro.MetroNoteEventBuffer;
import ats.metro.MetroNoteEventBufferSequence;

public class JavaPulsarLogic extends MetroLogic.Default {
    static void logInfo( Object msg ) {
    	System.err.println( msg );
		// Logger.getLogger(JavaPulsarLogic.class.getName()).log(Level.INFO, msg );
    }
    static void logError( String msg, Throwable e ) {
		Logger.getLogger(JavaPulsarLogic.class.getName()).log(Level.SEVERE, msg, e);
    }

	public JavaPulsarLogic() {
	}

	private boolean flag;
	@SuppressWarnings("unused")
	private void notifyFlag() {
		this.flag = true;
	}

	List<Pulsable> pulsableList = new ArrayList<Pulsable>(); 
	{
		logInfo(  "set-current-pulsable (from init)");
		setCurrentPulsable( new SamplePulsableBuilder() );
	}
	
	public void setCurrentPulsable( PulsableBuilder pulsableBuilder ) {
		logInfo(  "set current pulsable "  + pulsableBuilder.getName());
		pulsableList.clear();
		pulsableList.addAll( pulsableBuilder.create() );
	}
	
	@Override
	public void processInputMidiBuffer(Metro metro, List<MetroMidiEvent> in, List<MetroMidiEvent> out) {
		out.addAll( in );
		logInfo(  "in.size()" + in.size());
		logInfo(  "out.size()" + out.size());
	}


	@Override
	public boolean processOutputNoteBuffer( Metro metro, MetroNoteEventBufferSequence sequence, MetroNoteEventBuffer buf ) {
		// System.out.println("Metro.logic.new MetroLogic() {...}.initBuffer()" );

		buf.humanize( 0.0d, 3 );

		double maxBars = 0.0d;
		for ( Pulsable pulsable : pulsableList ) {
			pulsable.pulse( metro, sequence, buf );
			if ( maxBars < pulsable.getBarLength() )
				maxBars = pulsable.getBarLength();
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
			handle.spawn( "TEmp"/* FIXME */, 0.1d, new MetroLogic.Default() {
				int cnt = 2;
				@Override
				public boolean processOutputNoteBuffer(Metro metro, MetroNoteEventBufferSequence sequence, MetroNoteEventBuffer buf) {
					//				buf.noteShot( 0.5d  , 1 , 0, 57, 127 );

					buf.noteHit( 0.0d  , 1 , 0, 63, 127 );
					buf.noteHit( 0.2d  , 1 , 0, 63, 80 );
					buf.noteHit( 0.4d  , 1 , 0, 63, 80 );
					buf.noteHit( 0.6d  , 1 , 0, 63, 80 );
					buf.noteHit( 0.8d  , 1 , 0, 63, 80 );
					buf.length(1.0d);
					return 0<cnt--;
				}
				@Override
				public void processInputMidiBuffer(Metro metro, List<MetroMidiEvent> in, List<MetroMidiEvent> out) {
				}
			});
			flag = false;
		}
		
		return true;
	}
}
