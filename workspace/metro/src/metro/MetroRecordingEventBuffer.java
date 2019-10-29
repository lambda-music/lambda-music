package metro;

import java.util.Iterator;

public class MetroRecordingEventBuffer extends MidiReceiverDelegator<byte[]> implements Iterable<MetroEvent>, MetroMidiBufferedReceiver {
    public MetroRecordingEventBuffer(MetroMidiReceiver<byte[]> receiver) {
        super( receiver );
    }
    @Override
    public Iterator<MetroEvent> iterator() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void midiEvent(String id, double offset, MetroPort outputPort, byte[] data) {
        
    }
}
