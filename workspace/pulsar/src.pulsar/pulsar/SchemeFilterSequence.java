package pulsar;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.logging.Level;

import gnu.lists.EmptyList;
import gnu.lists.LList;
import gnu.lists.Pair;
import gnu.mapping.Procedure;
import gnu.mapping.Symbol;
import lamu.lib.Invokable;
import lamu.lib.kawautils.SchemeValues;
import lamu.lib.logging.Logger;
import metro.DirectMetroEventBuffer;
import metro.Metro;
import metro.MetroCollector;
import metro.MetroException;
import metro.MetroMidiEvent;
import metro.MetroPort;
import metro.MetroPortSelector;
import metro.MetroSequence;
import metro.MetroTrack;


public class SchemeFilterSequence implements MetroSequence, Invokable {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE,   msg, e   ); }
    static void logInfo (String msg             ) { LOGGER.log(Level.INFO,     msg      ); }
    static void logWarn (String msg             ) { LOGGER.log(Level.WARNING,  msg      ); }

    private MetroPortSelector portSelector= null;
    private SchemeSimpleMidiReceiver receiver;

    final Invokable invokable;
    final Runnable initializer;
    public SchemeFilterSequence( MetroPortSelector portSelector, Runnable initializer, Invokable invokable ) {
        this.portSelector = portSelector;
        this.initializer = initializer;
        this.invokable = invokable;
    }
    private final List<MetroPort>  inputPorts = new ArrayList<MetroPort>();
    private final List<MetroPort>  outputPorts = new ArrayList<MetroPort>();;
    public List<MetroPort> getInputPorts() {
        return inputPorts;
    }
    public List<MetroPort> getOutputPorts() {
        return outputPorts;
    }

    DirectMetroEventBuffer eventBuffer = new DirectMetroEventBuffer();
    static final Procedure reverse = (Procedure) gnu.kawa.slib.srfi1.reverse.get();
    boolean initialized=false;
    private void init( Metro metro, MetroTrack track ) {
        metro.referPorts( Arrays.asList(this.portSelector), this.inputPorts, this.outputPorts );
        this.receiver = new SchemeSimpleMidiReceiver(); 
        if ( 0 < outputPorts.size() )
            this.receiver.setPort( outputPorts.get(0) );

        this.initialized = true;
    }

    private volatile long totalCursor = 0;
    public long getTotalCursor() {
        return totalCursor;
    }
    
    @Override
    public void process(
        Metro metro, 
        MetroTrack track, 
        long nframes, 
        long measureLengthInFrames,
        List<MetroMidiEvent> inputMidiEvents, 
        List<MetroMidiEvent> outputMidiEvents, 
        List<MetroTrack> tracks,
        List<MetroTrack> registeringTracks, 
        List<MetroTrack> finalizingTracks, 
        List<MetroTrack> unregisteringTracks)
            throws MetroException 
    {
        if ( ! initialized ) 
            init( metro, track );

        try {
            LList inputNotations = EmptyList.emptyList;
            for ( MetroMidiEvent e : inputMidiEvents ) {
                if ( inputPorts.contains( e.getPort() ) ) {
                    LList list = this.receiver.receive( e );
                    if ( list != null ) {
                        inputNotations = Pair.make( list, inputNotations );
                    }
                    logInfo( list.toString() );
                }
            }
            
            Object result = invokable.invoke(  (Object)inputNotations );

            LList outputNotations;
            if ( result instanceof LList ) {
                outputNotations = (LList) result;
            } else {
                outputNotations = null;
            }

            if ( outputNotations != null ) {
                this.eventBuffer.setResultList( outputMidiEvents );
                for ( Object notation : outputNotations ) {
                    Object a = SchemeValues.alistGet( NoteListCommon.ID_OFFSET , (LList)notation, Boolean.FALSE );
                    if ( a instanceof Boolean ) {

                    } else {
                        PulsarNoteListParser.getInstance().parse( metro, null, (LList)notation, this.eventBuffer, MetroCollector.NULL );
                    }
                }
            }
        } finally {
            this.totalCursor += nframes;
        }
    }

    static final Symbol COMMAND_FILTER  = Symbol.valueOf( "filter" );
    static final Symbol COMMAND_SWITCH  = Symbol.valueOf( "filter" );
    static final Symbol recordingOn  = Symbol.valueOf( "rec-on" );
    static final Symbol recordingOff = Symbol.valueOf( "rec-off" );
    @Override
    public Object invoke(Object... args) {
        if ( 0 < args.length ) {
            if ( recordingOff.equals( args[0] ) ) {

            }
        }
        return null;
    }

}
