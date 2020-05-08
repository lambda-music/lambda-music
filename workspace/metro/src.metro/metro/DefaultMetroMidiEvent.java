package metro;

public class DefaultMetroMidiEvent implements MetroMidiEvent {
    public static DefaultMetroMidiEvent duplicate( MetroMidiEvent e ) {
        return new DefaultMetroMidiEvent(e);
    }
    public DefaultMetroMidiEvent(long midiOffset, MetroPort outputPort, byte[] midiData) {
        super();
        this.midiOffset = midiOffset;
        this.outputPort = outputPort;
        
        this.midiData = midiData;
        /* (Fri, 17 Apr 2020 01:35:29 +0900)
         * 
         * I think copying array is not necessary to prevend the weired midi data behavior problem.
         * But it is still necessary to be careful that.
         * this.midiData = copyArray( midiData );
         */

    }
    public DefaultMetroMidiEvent( MetroMidiEvent event) {
        this( event.getMidiOffset(), event.getPort(), event.getMidiData() );
    }
    
    static byte[] copyArray(byte[] arr) {
        byte[] newArr= new byte[ arr.length];
        System.arraycopy( arr ,0,  newArr,0, arr.length );
        return newArr;
    }

    private volatile long midiOffset;
    @Override
    public final long getMidiOffset() {
        return midiOffset;
    }
    @Override
    public final void setMidiOffset(long midiOffset) {
        this.midiOffset = midiOffset;
    }
    @Override
    public void moveMidiOffset(long offset) {
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
        MetroMidi midi = getMidi();
        return String.format( 
            "(MidiEvent offset:%3d port:%s data:%s)",
            getMidiOffset(),
            getPort(),
            midi.receiveMidi( 
                MetroMidiReceiver.FormatString.getInstance(), 
                this.getMidiData()));
    }
}
