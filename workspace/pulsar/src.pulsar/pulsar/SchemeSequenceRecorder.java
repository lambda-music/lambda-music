package pulsar;

import java.lang.invoke.MethodHandles;
import java.util.Collection;
import java.util.List;
import java.util.logging.Level;

import gnu.lists.EmptyList;
import gnu.lists.LList;
import gnu.lists.Pair;
import gnu.mapping.Procedure;
import gnu.mapping.Symbol;
import lamu.lib.Invokable;
import lamu.lib.kawautils.SchemeValues;
import lamu.lib.log.Logger;
import metro.Metro;
import metro.MetroCollector;
import metro.MetroException;
import metro.MetroMidiEvent;
import metro.MetroPort;
import metro.MetroReadable;
import metro.MetroTrack;
import metro.SimpleMetroEventBuffer;


public class SchemeSequenceRecorder extends MetroTrack implements MetroReadable, Invokable {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE,   msg, e   ); }
    static void logInfo (String msg             ) { LOGGER.log(Level.INFO,     msg      ); }
    static void logWarn (String msg             ) { LOGGER.log(Level.WARNING,  msg      ); }

    public static SchemeSequenceRecorder create(
        Object name, Collection<Object> tags,
            List<MetroPort> inputPorts, List<MetroPort> outputPorts, 
            double recordLength, boolean loop ) {
        return new SchemeSequenceRecorder( name, tags, inputPorts, outputPorts, recordLength, loop );
    }

    final SchemeSimpleMidiReceiver receiver;
    private List<MetroPort>  inputPorts;
    private List<MetroPort>  outputPorts;
    private double recordLength;
    @SuppressWarnings("unused")
    private boolean loop;
    private transient LList notations = EmptyList.emptyList;
    
    public SchemeSequenceRecorder(Object name, Collection<Object> tags, 
        List<MetroPort> inputPorts, List<MetroPort> outputPorts, double recordLength, boolean loop ) 
    {
        super(name, tags);
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
    public LList readContent() {
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
    public void processBuffer(Metro metro, long barLengthInFrames) throws MetroException {
    }

    private volatile long totalCursor = 0;
    @Override
    public void progressCursor(
        Metro metro, 
        long nframes, 
        List<MetroMidiEvent> inputMidiEventList,
        List<MetroMidiEvent> outputMidiEventList) throws MetroException 
    {
        try {
            long oneBarLengthInFrames = metro.getOneBarLengthInFrames();
            
            long currentPos;
            if ( 0 < this.recordLength ) {
                long recordingLengthInFrames = (long)(this.recordLength * oneBarLengthInFrames);
                currentPos = totalCursor % recordingLengthInFrames;
            } else {
                currentPos = totalCursor;
            }

            if ( recording ) {
                for ( MetroMidiEvent e : inputMidiEventList ) {
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
                this.eventBuffer.setResultList( outputMidiEventList );
                
                for ( Object notation : this.notations ) {
                    Object a = SchemeValues.alistGet( NoteListCommon.ID_OFFSET , (LList)notation, Boolean.FALSE );
                    if ( a instanceof Boolean ) {
                        
                    } else {
                        double d = SchemeValues.toDouble( a );
                        if ( from <= d && d < to ) {
                            logInfo( Double.toString( from ) );
                            // MOVED FROM SchemeSequence (Wed, 06 Nov 2019 17:07:05 +0900)
                            // MOVED AGAIN FROM NoteListParser (Thu, 02 Jan 2020 18:00:29 +0900)
                            PulsarNoteListParser.getInstance().parse( metro, null, (LList)notation, this.eventBuffer, MetroCollector.NULL );
                        }
                    }
                }
            }
        } catch (MetroException e) {
            logError( "could not get bar length : failed to send midi messages.", e );
        } finally {
            this.totalCursor += nframes;
        }
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
