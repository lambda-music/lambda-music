package ats.pulsar.old;

import java.util.ArrayList;
import java.util.List;

import ats.metro.MetroLogic;
import ats.metro.MetroMidiEvent;
import ats.metro.MetroNoteEventBuffer;
import ats.pulsar.Pulsable;
import ats.pulsar.PulsableBuilder;

public class JavaPulsarLogic extends MetroLogic.Default {
	public JavaPulsarLogic() {
	}

	private boolean flag;
	private void notifyFlag() {
		this.flag = true;
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
			handle.spawn( "TEmp"/* FIXME */, 0.1d, new MetroLogic.Default() {
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
}
