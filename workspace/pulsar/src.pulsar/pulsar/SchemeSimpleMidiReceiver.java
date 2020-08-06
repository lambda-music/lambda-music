package pulsar;

import gnu.lists.LList;
import metro.MetroMidiEvent;
import metro.MetroMidiMessages;
import metro.MetroDirectToBufferedMidiReceiver;
import metro.MetroPort;

public class SchemeSimpleMidiReceiver {
    public static MetroDirectToBufferedMidiReceiver<LList,LList> createReceiver() {
        return MetroDirectToBufferedMidiReceiver.createRecorder( SchemeBufferedMidiReceiver.getInstance() );
    }

    private MetroDirectToBufferedMidiReceiver<LList,LList> receiver =  SchemeSimpleMidiReceiver.createReceiver();
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
        LList list = MetroMidiMessages.receive( this.receiver, e.getMidiData() );
        return list;
    }
}
