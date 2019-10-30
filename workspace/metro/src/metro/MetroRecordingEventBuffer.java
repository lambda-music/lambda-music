package metro;

import java.util.Iterator;

import gnu.lists.LList;

public class MetroRecordingEventBuffer extends MetroNonBufferedToBufferedMidiReceiver<byte[],LList> implements Iterable<MetroEvent> {
    public MetroRecordingEventBuffer(MetroBufferedMidiReceiver<LList> receiver) {
        super( receiver );
    }
    @Override
    public Iterator<MetroEvent> iterator() {
        return null;
    }

}
