package pulsar;

import java.lang.invoke.MethodHandles;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

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
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE,   msg, e   ); }
    static void logInfo (String msg             ) { LOGGER.log(Level.INFO,     msg      ); }
    static void logInfo (Object msg             ) { LOGGER.log(Level.INFO,"{0}",msg      ); }
    static void logWarn (String msg             ) { LOGGER.log(Level.WARNING,  msg      ); }

    public static SchemeSequenceRecorder createSchemeSequenceRecorder(
            List<MetroPort> inputPorts, List<MetroPort> outputPorts, int recordLength, boolean loop) {
        return new SchemeSequenceRecorder( inputPorts, outputPorts, recordLength, loop );
    }

    final MetroMidiReceiverBufferer<LList,LList> receiver;
    private List<MetroPort>  inputPorts;
    private List<MetroPort>  outputPorts;
    private int recordLength;
    private boolean loop;
    private transient LList data = EmptyList.emptyList;
    private SchemeSequenceRecorder( List<MetroPort> inputPorts, List<MetroPort> outputPorts, int recordLength, boolean loop ) {
        this.inputPorts = inputPorts;
        this.outputPorts = outputPorts;
        this.recordLength = recordLength;
        this.loop = loop;
        this.receiver =  SchemeMidiRecorder.create();
        if ( 0 < outputPorts.size() )
            this.receiver.setPort( outputPorts.get(0) );
    }
    public List<MetroPort> getInputPorts() {
        return inputPorts;
    }
    public List<MetroPort> getOutputPorts() {
        return outputPorts;
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
            if ( inputPorts.contains( e.getPort() ) ) {
                this.receiver.setOffset( ( (double)(totalCursor + e.getMidiOffset() ) / 44100.0d ) );
                LList list = MetroMidiMessageGen.receive( receiver, e.getMidiData() );
                this.data = Pair.make( list, data );
                logInfo( list );
            }
        }
    }

    @Override
    public boolean processBuffered(Metro metro, MetroTrack track, MetroEventBuffer buf) {
        return true;
    }
    
}
