package pulsar;

import gnu.lists.LList;
import metro.MetroPort;
import metro.MetroSequenceRecorder;

public class SchemeSequenceRecorder extends MetroSequenceRecorder implements ReadableSchemeSequence {
    public SchemeSequenceRecorder(int recordLength, boolean looper, MetroPort inputPort, MetroPort outputPort) {
        super( recordLength, looper, inputPort, outputPort );
    }

    @Override
    public LList readMusic() {
        return null;
    }
    
}
