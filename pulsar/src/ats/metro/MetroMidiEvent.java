package ats.metro;

import java.util.Comparator;

public class MetroMidiEvent implements Comparable<MetroMidiEvent> {
	public static final Comparator<? super MetroMidiEvent> COMPARATOR = new Comparator<MetroMidiEvent>( ) {
		@Override
		public int compare(MetroMidiEvent o1, MetroMidiEvent o2) {
			return o1.compareTo( o2 );
		}
	};

	private int outputPortNo;
	private int offset;
	private byte[] data;
	public MetroMidiEvent(int outputPortNo, int offset, byte[] data ) {
		this.outputPortNo = outputPortNo;
		this.offset = offset;
		this.data = data;
	}
	public int getOutputPortNo() {
		return outputPortNo;
	}
	public void setOutputPortNo(int outputPortNo) {
		this.outputPortNo = outputPortNo;
	}
	public int getOffset() {
		return offset;
	}
	public void setOffset(int offset) {
		this.offset = offset;
	}
	public byte[] getData() {
		return data;
	}
	public void setData(byte[] data) {
		this.data = data;
	}
	@Override
	public int compareTo(MetroMidiEvent o) {
		return this.offset - o.offset;
	}
	
	@Override
	public String toString() {
		return "offset : " + Integer.toString( this.offset ) ;
	}
}
