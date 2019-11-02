package pulsar;

import java.util.List;

import gnu.lists.EmptyList;
import gnu.lists.LList;
import gnu.lists.Pair;
import gnu.mapping.Procedure;
import metro.Metro;
import metro.MetroEventBuffer;
import metro.MetroMidiEvent;
import metro.MetroMidiMessageGen;
import metro.MetroMidiReceiverBufferer;
import metro.MetroPort;
import metro.MetroSequence;
import metro.MetroTrack;


public class SchemeSequenceRecorder implements MetroSequence, ReadableSchemeSequence {
    final MetroMidiReceiverBufferer<LList,LList> receiver;
    private MetroPort inputPort;
    private MetroPort outputPort;
    private int recordLength;
    private boolean loop;
    private transient LList data = EmptyList.emptyList;
    public SchemeSequenceRecorder( MetroPort inputPort, MetroPort outputPort, int recordLength, boolean loop ) {
        this.inputPort = inputPort;
        this.outputPort = outputPort;
        this.recordLength = recordLength;
        this.loop = loop;
        this.receiver =  SchemeMidiRecorder.create();
        this.receiver.setPort( outputPort );
    }

    static final Procedure reverse = (Procedure) gnu.kawa.slib.srfi1.reverse.get();
    @Override
    public LList readMusic() {
        try {
            return (LList)reverse.apply1( data );
        } catch (Throwable e) {
            throw new RuntimeException( e );
        }
    }

    @Override
    public void processDirect(Metro metro, int totalCursor, List<MetroMidiEvent> in, List<MetroMidiEvent> out) {
        for ( MetroMidiEvent e : in ) {
            this.receiver.setOffset( ( (double)(totalCursor + e.getMidiOffset() ) / 44100.0d ) );
            LList list = MetroMidiMessageGen.receive( receiver, e.getMidiData() );
            this.data = Pair.make( list, data );
            System.out.println( list );
        }
    }

    @Override
    public boolean processBuffered(Metro metro, MetroTrack track, MetroEventBuffer buf) {
        return true;
    }
    
}
