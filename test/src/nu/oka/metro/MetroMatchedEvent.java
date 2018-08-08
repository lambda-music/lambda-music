package nu.oka.metro;

import java.util.Comparator;

public class MetroMatchedEvent implements Comparable<MetroMatchedEvent> {
	public static final Comparator<? super MetroMatchedEvent> COMPARATOR = new Comparator<MetroMatchedEvent>( ) {
		@Override
		public int compare(MetroMatchedEvent o1, MetroMatchedEvent o2) {
			return o1.compareTo( o2 );
		}
	};

	private int outputPortNo;
	private int offset;
	private byte[] data;
	public MetroMatchedEvent(int outputPortNo, int offset, byte[] data ) {
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
	public int compareTo(MetroMatchedEvent o) {
		return this.offset - o.offset;
	}
	
	@Override
	public String toString() {
		return "offset : " + Integer.toString( this.offset ) ;
	}
}
