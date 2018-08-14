package ats.metro;

import java.util.Arrays;

public class MetroNoteEvent {
	int outputPortNo;
	double offset;
	int offsetInFrames;
	byte[] data;
	
	public MetroNoteEvent(int outputPortNo, double offset, byte[] data) {
		super();
		this.outputPortNo = outputPortNo;
		this.offset = offset;
		this.data = data;
	}
	
	public int getOutputPortNo() {
		return outputPortNo;
	}
	public double getOffset() {
		return offset;
	}
	public int getOffsetInFrames() {
		return offsetInFrames;
	}
	public byte[] getData() {
		return data;
	}
	public final void calcInFrames( int barInFrames ){
		this.offsetInFrames = (int)(this.offset * barInFrames );
	}
	public boolean between(int from, int to ) {
		return from <= this.offsetInFrames && this.offsetInFrames < to;
	}

	public String dump(String string) {
		MetroNoteEvent e = this;
		StringBuilder sb = new StringBuilder();
		sb.append(string).append( "      outputPortNo: " + e.outputPortNo ).append( "\n" );
		sb.append(string).append( "            offset: " + e.offset ).append( "\n" );
		sb.append(string).append( "    offsetInFrames: " + e.offsetInFrames ).append( "\n" );
		sb.append(string).append( "              data: " + Arrays.toString( e.data ) ).append( "\n" );
		return sb.toString();
	}
//	public static void main(String[] args) {
//		MetroMidiEvent event = new MetroMidiEvent(1, 0.0d, new byte[] {} );
//		event.calcInFrames(48000);
//		boolean b = event.between(-1, 4);
//		System.out.println( b );
//	}
}