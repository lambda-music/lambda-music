package ats.metro;

public final class MetroStaticMidiEvent implements MetroAbstractMidiEvent {
	private int outputPortNo;
	private int midiOffset;
	private byte[] midiData;
	public MetroStaticMidiEvent( int outputPortNo, int offset, byte[] data ) {
		this.outputPortNo = outputPortNo;
		this.midiOffset = offset;
		this.midiData = data;
	}
	@Override
	public int getOutputPortNo() {
		return outputPortNo;
	}
	public void setOutputPortNo( int outputPortNo ) {
		this.outputPortNo = outputPortNo;
	}
	@Override
	public int getMidiOffset() {
		return midiOffset;
	}
	public void setMidiOffset(int midiOffset) {
		this.midiOffset = midiOffset;
	}
	@Override
	public byte[] getMidiData() {
		return midiData;
	}
	public void setMidiData(byte[] midiData) {
		this.midiData = midiData;
	}
	@Override
	public String toString() {
		return "offset : " + Integer.toString( this.midiOffset ) ;
	}
}
