package pulsar;

import java.lang.invoke.MethodHandles;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.jaudiolibs.jnajack.JackException;

import gnu.lists.EmptyList;
import gnu.lists.LList;
import gnu.lists.Pair;
import gnu.mapping.Procedure;
import gnu.mapping.Symbol;
import metro.Metro;
import metro.MetroEventBuffer;
import metro.MetroMidiEvent;
import metro.MetroMidiMessageGen;
import metro.MetroMidiReceiverBufferer;
import metro.MetroPort;
import metro.MetroSequence;
import metro.MetroTrack;
import pulsar.lib.secretary.Invokable;


public class SchemeSequenceRecorder implements MetroSequence, SchemeSequenceReadable, Invokable {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE,   msg, e   ); }
    static void logInfo (String msg             ) { LOGGER.log(Level.INFO,     msg      ); }
    static void logInfo (Object msg             ) { LOGGER.log(Level.INFO,"{0}",msg      ); }
    static void logWarn (String msg             ) { LOGGER.log(Level.WARNING,  msg      ); }

    public static SchemeSequenceRecorder createSchemeSequenceRecorder(
            List<MetroPort> inputPorts, List<MetroPort> outputPorts, 
            double recordLength, boolean loop ) {
        return new SchemeSequenceRecorder( inputPorts, outputPorts, recordLength, loop );
    }

    final MetroMidiReceiverBufferer<LList,LList> midi2schemeReceiver;
    private List<MetroPort>  inputPorts;
    private List<MetroPort>  outputPorts;
    private double recordLength;
    private boolean loop;
    private transient LList data = EmptyList.emptyList;
    private SchemeSequenceRecorder( List<MetroPort> inputPorts, List<MetroPort> outputPorts, double recordLength, boolean loop ) {
        this.inputPorts = inputPorts;
        this.outputPorts = outputPorts;
        this.recordLength = recordLength;
        this.loop = loop;
        this.midi2schemeReceiver =  SchemeMidiRecorder.create();
        if ( 0 < outputPorts.size() )
            this.midi2schemeReceiver.setPort( outputPorts.get(0) );
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

    private boolean recording = true;
    public void setRecording(boolean recording) {
        this.recording = recording;
    }
    public boolean isRecording() {
        return recording;
    }
    private boolean playing = true;
    public boolean isPlaying() {
        return playing;
    }
    public void setPlaying(boolean playing) {
        this.playing = playing;
    }
    @Override
    public void processDirect(Metro metro, int totalCursor, List<MetroMidiEvent> in, List<MetroMidiEvent> out) {
        try {
            int oneBarLengthInFrames = metro.getOneBarLengthInFrames();
            
            int currentPos;
            if ( 0 < this.recordLength ) {
                int recordingLengthInFrames = (int)(this.recordLength * oneBarLengthInFrames);
                currentPos = totalCursor % recordingLengthInFrames;
            } else {
                currentPos = totalCursor;
            }

            if ( recording ) {
                for ( MetroMidiEvent e : in ) {
                    if ( inputPorts.contains( e.getPort() ) ) {
                        this.midi2schemeReceiver.setOffset( ( (double)(currentPos + e.getMidiOffset() ) / (double)oneBarLengthInFrames ) );
                        LList list = MetroMidiMessageGen.receive( midi2schemeReceiver, e.getMidiData() );
                        this.data = Pair.make( list, data );
                        logInfo( list );
                    }
                }
            }
            
            if ( playing ) {
                
            }
        } catch (JackException e) {
            logError( "could not get bar length : failed to send midi messages.", e );
        }
    }

    @Override
    public boolean processBuffered(Metro metro, MetroTrack track, MetroEventBuffer buf) {
        return true;
    }
    
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
