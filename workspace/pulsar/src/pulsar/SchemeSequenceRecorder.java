package pulsar;

import java.lang.invoke.MethodHandles;
import java.util.List;
import java.util.logging.Level;

import org.jaudiolibs.jnajack.JackException;

import gnu.lists.EmptyList;
import gnu.lists.LList;
import gnu.lists.Pair;
import gnu.mapping.Procedure;
import gnu.mapping.Symbol;
import lamu.lib.evaluators.Invokable;
import lamu.lib.evaluators.SchemeUtils;
import lamu.lib.log.Logger;
import metro.Metro;
import metro.MetroBufferedMidiReceiver;
import metro.MetroCollector;
import metro.MetroMidiEvent;
import metro.MetroPort;
import metro.MetroSequence;
import metro.MetroTrack;
import metro.SimpleMetroEventBuffer;


public class SchemeSequenceRecorder implements MetroSequence, SchemeSequenceReadable, Invokable {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE,   msg, e   ); }
    static void logInfo (String msg             ) { LOGGER.log(Level.INFO,     msg      ); }
    static void logWarn (String msg             ) { LOGGER.log(Level.WARNING,  msg      ); }

    public static SchemeSequenceRecorder createSchemeSequenceRecorder(
            List<MetroPort> inputPorts, List<MetroPort> outputPorts, 
            double recordLength, boolean loop ) {
        return new SchemeSequenceRecorder( inputPorts, outputPorts, recordLength, loop );
    }

    final SchemeSimpleMidiReceiver receiver;
    private List<MetroPort>  inputPorts;
    private List<MetroPort>  outputPorts;
    private double recordLength;
    @SuppressWarnings("unused")
    private boolean loop;
    private transient LList notations = EmptyList.emptyList;
    private SchemeSequenceRecorder( List<MetroPort> inputPorts, List<MetroPort> outputPorts, double recordLength, boolean loop ) {
        this.inputPorts = inputPorts;
        this.outputPorts = outputPorts;
        this.recordLength = recordLength;
        this.loop = loop;
        this.receiver = new SchemeSimpleMidiReceiver(); 
        if ( 0 < outputPorts.size() )
            this.receiver.setPort( outputPorts.get(0) );
    }
    public List<MetroPort> getInputPorts() {
        return inputPorts;
    }
    public List<MetroPort> getOutputPorts() {
        return outputPorts;
    }
    
    SimpleMetroEventBuffer eventBuffer = new SimpleMetroEventBuffer();

    static final Procedure reverse = (Procedure) gnu.kawa.slib.srfi1.reverse.get();
    @Override
    public LList readMusic() {
        try {
            return (LList)reverse.apply1( notations );
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
    public void processDirect(Metro metro, int nframes, int totalCursor, List<MetroMidiEvent> in, List<MetroMidiEvent> out) {
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
                        LList list = this.receiver.receive( e, currentPos, oneBarLengthInFrames );
                        if ( list != null ) {
                            this.notations = Pair.make( list, notations );
                        }
                        logInfo( list.toString() );
                    }
                }
            }
            
            if ( playing ) {
                double from = ((double)totalCursor           ) / (double)oneBarLengthInFrames;
                double to   = ((double)totalCursor + nframes ) / (double)oneBarLengthInFrames;
                
                this.eventBuffer.setCursorOffset( totalCursor );
                this.eventBuffer.setOneBarLengthInFrames( oneBarLengthInFrames );
                this.eventBuffer.setResultList( out );
                
                for ( Object notation : this.notations ) {
                    Object a = SchemeUtils.alistGet( NoteListCommon.ID_OFFSET , (LList)notation, Boolean.FALSE );
                    if ( a instanceof Boolean ) {
                        
                    } else {
                        double d = SchemeUtils.toDouble( a );
                        if ( from <= d && d < to ) {
                            logInfo( Double.toString( from ) );
                            // MOVED FROM SchemeSequence (Wed, 06 Nov 2019 17:07:05 +0900)
                            // MOVED AGAIN FROM NoteListParser (Thu, 02 Jan 2020 18:00:29 +0900)
                            PulsarNoteListParser.getInstance().parse( metro, null, (LList)notation, this.eventBuffer, MetroCollector.NULL );
                        }
                    }
                }
            }
        } catch (JackException e) {
            logError( "could not get bar length : failed to send midi messages.", e );
        }
    }

    @Override
    public <T> void processBuffered(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T> buffer) {
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
