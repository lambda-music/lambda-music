package ats.metro;

import java.util.Arrays;

public class MetroAbstractMidiEvent extends MetroAbstractEvent {
	final int outputPortNo;
	byte[] data;
	public MetroAbstractMidiEvent( double offset, int outputPortNo, byte[] data ) {
		super( offset );
		this.outputPortNo = outputPortNo;
		this.data = data;
	}
	public final int getOutputPortNo() {
		return outputPortNo;
	}
	public byte[] getData() {
		return data;
	}
	public void dumpProc( String prefix, StringBuilder sb ) {
		super.dumpProc(prefix, sb);
		sb.append(prefix).append( "      outputPortNo: " + outputPortNo ).append( "\n" );
		sb.append(prefix).append( "              data: " + Arrays.toString( data ) ).append( "\n" );
	}


//	public static void main(String[] args) {
//		MetroMidiEvent event = new MetroMidiEvent(1, 0.0d, new byte[] {} );
//		event.calcInFrames(48000);
//		boolean b = event.between(-1, 4);
//		System.out.println( b );
//	}
}