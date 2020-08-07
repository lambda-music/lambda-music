package pulsar;

import gnu.lists.LList;
import metro.MetroDirectToBufferedMidiReceiver;
import metro.MetroMidiEvent;
import metro.MetroMidiMessages;
import metro.MetroPort;

public class SchemeSimpleMidiReceiver {
    private static MetroDirectToBufferedMidiReceiver<LList,LList> createReceiver() {
        return MetroDirectToBufferedMidiReceiver.createDefault( SchemeBufferedMidiReceiver.getInstance() );
    }
    private MetroDirectToBufferedMidiReceiver<LList,LList> receiver =  SchemeSimpleMidiReceiver.createReceiver();
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
    public LList receive(MetroMidiEvent e ) {
        return MetroMidiMessages.receive( this.receiver, e.getMidiData() );
    }
}
