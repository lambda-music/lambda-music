package pulsar;

import gnu.lists.LList;
import metro.MetroMidiReceiverBufferer;

public class SchemeMidiRecorder {
    public static MetroMidiReceiverBufferer<LList,LList> create() {
        return MetroMidiReceiverBufferer.createRecorder( SchemeBufferedMidiReceiver.getInstance() );
    }
}
