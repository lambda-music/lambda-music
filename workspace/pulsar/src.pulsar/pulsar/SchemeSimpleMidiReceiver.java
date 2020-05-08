package pulsar;

import gnu.lists.LList;
import metro.MetroMidiEvent;
import metro.MetroMidiMessageGen;
import metro.MetroMidiReceiverBufferer;
import metro.MetroPort;

public class SchemeSimpleMidiReceiver {
    public static MetroMidiReceiverBufferer<LList,LList> createReceiver() {
        return MetroMidiReceiverBufferer.createRecorder( SchemeBufferedMidiReceiver.getInstance() );
    }

    private MetroMidiReceiverBufferer<LList,LList> receiver =  SchemeSimpleMidiReceiver.createReceiver();
    public SchemeSimpleMidiReceiver( MetroPort outputPort ) {
        receiver.setPort( outputPort );
    }
    public SchemeSimpleMidiReceiver() {
    }
    public void setPort(MetroPort port) {
        this.receiver.setPort( port );
    }

    public LList receive(MetroMidiEvent e, long currentPos, long oneBarLengthInFrames ) {
        this.receiver.setOffset( ( (double)(currentPos + e.getMidiOffset() ) / (double)oneBarLengthInFrames ) );
        LList list = MetroMidiMessageGen.receive( this.receiver, e.getMidiData() );
        return list;
    }
}
