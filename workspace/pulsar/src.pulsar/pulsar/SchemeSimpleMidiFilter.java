package pulsar;

import gnu.lists.LList;
import metro.MetroBufferedMidiReceiver;
import metro.MetroCollector;
import metro.MetroDirectToBufferedMidiReceiver;
import metro.MetroMidiEvent;
import metro.MetroMidiMessages;
import metro.MetroPort;

public class SchemeSimpleMidiFilter {
    private  static MetroDirectToBufferedMidiReceiver<LList,LList> createReceiver(MetroBufferedMidiReceiver<LList> filterReceiver) {
        return MetroDirectToBufferedMidiReceiver.createDefault( 
            SchemeBufferedMidiReceiver.getInstance() );
    }
    private MetroDirectToBufferedMidiReceiver<LList,LList> receiver;
    public SchemeSimpleMidiFilter( MetroBufferedMidiReceiver<LList> filterReceiver ) {
        this.receiver =  SchemeSimpleMidiFilter.createReceiver( filterReceiver );
    }
    public void setPort(MetroPort port) {
        this.receiver.setPort( port );
    }

    public void receive( MetroCollector<LList> result, MetroMidiEvent e, long currentPos, long oneBarLengthInFrames ) {
        this.receiver.setOffset( ( (double)(currentPos + e.getMidiOffset() ) / (double)oneBarLengthInFrames ) );
        MetroMidiMessages.receive( this.receiver, e.getMidiData(), result );
    }
}
