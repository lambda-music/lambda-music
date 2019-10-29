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
    MetroPort outputPort;
    @Override
    public final MetroPort getOutputPort() {
        return outputPort;
    }
    @Override
    public void setOutputPort(MetroPort outputPort) {
        this.outputPort = outputPort;
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
