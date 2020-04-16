package metro;

import metro.MetroMidiReceiver.FormatString;

public class DefaultMetroMidiEvent implements MetroMidiEvent {
    public static DefaultMetroMidiEvent duplicate( MetroMidiEvent e ) {
        return new DefaultMetroMidiEvent(e);
    }
    public DefaultMetroMidiEvent(int midiOffset, MetroPort outputPort, byte[] midiData) {
        super();
        this.midiOffset = midiOffset;
        this.outputPort = outputPort;
        this.midiData = copyArray( midiData );
    }
    public DefaultMetroMidiEvent( MetroMidiEvent event) {
        this( event.getMidiOffset(), event.getPort(), event.getMidiData() );
    }
    
    private static byte[] copyArray(byte[] arr) {
        byte[] newArr= new byte[ arr.length];
        System.arraycopy( arr ,0,  newArr,0, arr.length );
        return newArr;
    }

    private volatile int midiOffset;
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

    private volatile MetroPort outputPort;
    @Override
    public final MetroPort getPort() {
        return outputPort;
    }
    @Override
    public void setPort(MetroPort port) {
        this.outputPort = port;
    }

    private volatile byte[] midiData;
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
        MetroMidi midi = MetroMidi.getMidi( MetroMidi.getMidiCommand( getMidiData()));
        return String.format( 
            "(MidiEvent offset:%3d port:%s data:%s)",
            getMidiOffset(),
            getPort(),
            midi.receiveMidi( FormatString.getInstance(), this.getMidiData() ));
    }
}
