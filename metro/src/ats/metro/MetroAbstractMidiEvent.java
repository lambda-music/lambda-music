package ats.metro;

import java.util.Arrays;
import java.util.List;

public class MetroAbstractMidiEvent extends MetroAbstractEvent implements AbstractMidiEvent {
	final int outputPortNo;
	byte[] midiData;
	public MetroAbstractMidiEvent( double offset, int outputPortNo, byte[] data ) {
		super( offset );
		this.outputPortNo = outputPortNo;
		this.midiData = data;
	}
//	@Override
//	public final int getMidiOffset() {
//		return super.getMidiOffset();
//	}
	@Override
	public final int getOutputPortNo() {
		return outputPortNo;
	}
	@Override
	public byte[] getMidiData() {
		return midiData;
	}
	@Override
	public void process(Metro metro, int from, int to, int nframes, List<AbstractMidiEvent> eventList) {
		this.setMidiOffset( this.barOffsetInFrames - from );
//		eventList.add( new MetroMidiEvent(  
//				this.getOutputPortNo(), 
//				this.getMidiOffset(), 
//				this.getMidiData() 
//				));
		eventList.add( this );
	}
	
	public void dumpProc( String prefix, StringBuilder sb ) {
		super.dumpProc(prefix, sb);
		sb.append(prefix).append( "      outputPortNo: " + outputPortNo ).append( "\n" );
		sb.append(prefix).append( "              data: " + Arrays.toString( midiData ) ).append( "\n" );
	}


//	public static void main(String[] args) {
//		MetroMidiEvent event = new MetroMidiEvent(1, 0.0d, new byte[] {} );
//		event.calcInFrames(48000);
//		boolean b = event.between(-1, 4);
//		System.out.println( b );
//	}
}