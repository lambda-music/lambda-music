package ats.metro;

import java.util.Arrays;
import java.util.List;

public class MetroMidiEvent extends MetroEvent implements MetroAbstractMidiEvent {
	final int outputPortNo;
	byte[] midiData;
	public MetroMidiEvent( double offset, int outputPortNo, byte[] data ) {
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
	public void process(Metro metro, int from, int to, int nframes, List<MetroAbstractMidiEvent> eventList) {
		this.setMidiOffset( this.barOffsetInFrames - from );
		eventList.add( this );
	}
	
	public void dumpProc( String prefix, StringBuilder sb ) {
		super.dumpProc(prefix, sb);
		sb.append(prefix).append( "      outputPortNo: " + outputPortNo ).append( "\n" );
		sb.append(prefix).append( "              data: " + Arrays.toString( midiData ) ).append( "\n" );
	}
}