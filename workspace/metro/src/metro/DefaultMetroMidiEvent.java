package metro;

public class DefaultMetroMidiEvent implements MetroMidiEvent {
    public static DefaultMetroMidiEvent duplicate( MetroMidiEvent e ) {
        return new DefaultMetroMidiEvent(e);
    }
    public DefaultMetroMidiEvent(int midiOffset, MetroPort outputPort, byte[] midiData) {
        super();
        this.midiOffset = midiOffset;
        this.outputPort = outputPort;
        this.midiData = midiData;
    }
    public DefaultMetroMidiEvent( MetroMidiEvent event) {
        this( event.getMidiOffset(), event.getPort(), event.getMidiData() );
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
    @Override
    public String toString() {
        return String.format( 
            "(MidiEvent offset:%d port:%s ch:%d data:%s)",
                    getMidiOffset(),
                    getPort(),
                    MetroMidi.getMidiChannel( getMidiData()),
                    MetroMidi.getMidi( MetroMidi.getMidiCommand( getMidiData())));
    }
}
