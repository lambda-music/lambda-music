package metro;

public class DefaultMetroMidiEvent implements MetroMidiEvent {
    public DefaultMetroMidiEvent(int midiOffset, MetroPort outputPort, byte[] midiData) {
        super();
        this.midiOffset = midiOffset;
        this.outputPort = outputPort;
        this.midiData = midiData;
    }
    
    private int midiOffset;
    @Override
    public final int getMidiOffset() {
        return midiOffset;
    }
    @Override
    public final void setMidiOffset(int midiOffset) {
        this.midiOffset = midiOffset;
    }
    @Override
    public void moveMidiOffset(int offset) {
        this.midiOffset += offset;
    }

    MetroPort outputPort;
    @Override
    public final MetroPort getPort() {
        return outputPort;
    }
    @Override
    public void setPort(MetroPort port) {
        this.outputPort = port;
    }

    byte[] midiData;
    @Override
    public byte[] getMidiData() {
        return midiData;
    }
    @Override
    public void setMidiData(byte[] midiData) {
        this.midiData = midiData;
    }
}
