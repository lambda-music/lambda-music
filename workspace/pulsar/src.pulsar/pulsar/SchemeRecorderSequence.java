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
import metro.Metro;
import metro.MetroCollector;
import metro.MetroException;
import metro.MetroMidiEvent;
import metro.MetroPort;
import metro.MetroPortSelector;
import metro.MetroReadable;
import metro.MetroSequence;
import metro.MetroTrack;
import metro.SimpleMetroEventBuffer;


public class SchemeRecorderSequence implements MetroSequence, MetroReadable, Invokable {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE,   msg, e   ); }
    static void logInfo (String msg             ) { LOGGER.log(Level.INFO,     msg      ); }
    static void logWarn (String msg             ) { LOGGER.log(Level.WARNING,  msg      ); }

    public static SchemeRecorderSequence create( MetroPortSelector portSelector, double recordLength, boolean loop ) {
        return new SchemeRecorderSequence( portSelector, recordLength, loop );
    }

    private MetroPortSelector portSelector= null;
    private SchemeSimpleMidiReceiver receiver;
    private final List<MetroPort>  inputPorts = new ArrayList<MetroPort>();
    private final List<MetroPort>  outputPorts = new ArrayList<MetroPort>();;
    private double recordLength;
    @SuppressWarnings("unused")
    private boolean loop;
    private volatile LList notations = EmptyList.emptyList;

    public SchemeRecorderSequence( MetroPortSelector portSelector, double recordLength, boolean loop ) {
		this.portSelector = portSelector;
        this.recordLength = recordLength;
        this.loop = loop;
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
	boolean initialized=false;
    private void init( Metro metro, MetroTrack track ) {
    	metro.referPorts( Arrays.asList(this.portSelector), this.inputPorts, this.outputPorts );
    	this.receiver = new SchemeSimpleMidiReceiver(); 
        if ( 0 < outputPorts.size() )
            this.receiver.setPort( outputPorts.get(0) );
        
        this.initialized = true;
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

    private volatile long totalCursor = 0;
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
            long currentPos;
            if ( 0 < this.recordLength ) {
                long recordingLengthInFrames = (long)(this.recordLength * measureLengthInFrames);
                currentPos = totalCursor % recordingLengthInFrames;
            } else {
                currentPos = totalCursor;
            }

            if ( recording ) {
                for ( MetroMidiEvent e : inputMidiEvents ) {
                    if ( inputPorts.contains( e.getPort() ) ) {
                        LList list = this.receiver.receive( e, currentPos, measureLengthInFrames );
                        if ( list != null ) {
                            this.notations = Pair.make( list, notations );
                        }
                        logInfo( list.toString() );
                    }
                }
            }
            
            if ( playing ) {
                double from = ((double)totalCursor           ) / (double)measureLengthInFrames;
                double to   = ((double)totalCursor + nframes ) / (double)measureLengthInFrames;
                
                this.eventBuffer.setCursorOffset( totalCursor );
                this.eventBuffer.setOneBarLengthInFrames( measureLengthInFrames );
                this.eventBuffer.setResultList( outputMidiEvents );
                
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
