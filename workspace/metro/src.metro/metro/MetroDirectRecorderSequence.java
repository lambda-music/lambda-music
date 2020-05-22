package metro;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class MetroDirectRecorderSequence implements MetroSequence {
    public MetroDirectRecorderSequence( int recordLength, boolean looper, MetroPort inputPort, MetroPort outputPort ) {
        this.recordLength = recordLength;
        this.looper = looper;
        this.inputPort = inputPort;
        this.outputPort = outputPort;
    }
    private MetroPort inputPort;
    private MetroPort outputPort;
    public MetroPort getInputPort() {
        return inputPort;
    }
    public MetroPort getOutputPort() {
        return outputPort;
    }
    
    public static enum Mode { RECORD, PLAY }
    private Mode mode = Mode.RECORD;
    public Mode getMode() {
        return mode;
    }

    public void setMode(Mode mode) {
        this.mode = mode;
    }

    boolean looper = false;
    public void setLooper(boolean repeatPlay) {
        this.looper = repeatPlay;
    }
    public boolean isLooper() {
        return looper;
    }
    
    int recordLength = -1;
    public int getRecordLength() {
        return recordLength;
    }
    public void setRecordLength(int recordLength) {
        this.recordLength = recordLength;
    }

    private List<MetroMidiFrame> frames = new ArrayList<>();
    public List<MetroMidiFrame> getFrames() {
        return frames;
    }
    private Iterator<MetroMidiFrame> player = null;

    private volatile int totalCursor = 0;
    
    @Override
    public void progressBuffer(Metro metro, MetroTrack track, long measureLengthInFrames) throws MetroException {
    }
    @Override
    public void progressCursor(
        Metro metro, MetroTrack track, 
        long nframes,
        long measureLengthInFrames, List<MetroMidiEvent> inputMidiEvents, List<MetroMidiEvent> outputMidiEvents, List<MetroTrack> tracks, List<MetroTrack> registeringTracks, List<MetroTrack> unregisteringTracks ) throws MetroException 
    {
        try {
            switch ( this.mode ) {
            case PLAY:
                if ( player == null ) {
                    player = frames.iterator();
                }
                
                if ( ! player.hasNext() ) {
                    if ( isLooper() ) {
                        player = frames.iterator();
                    }
                }
                
                if ( ! player.hasNext() ) {
                    outputMidiEvents.addAll( player.next().getMidiEventList() );
                }
                
                break;
            case RECORD:
                if ( 0< this.recordLength && this.recordLength < totalCursor ) {
                    this.setMode( Mode.PLAY );
                } else {
                    ArrayList<MetroMidiEvent> list = new ArrayList<>();
                    for ( MetroMidiEvent e : inputMidiEvents ) {
                        if ( e.getPort().equals( this.inputPort ) ) {
                            e.setPort( this.outputPort );
                            list.add(e);
                        }
                    }
                    frames.add( new MetroMidiFrame( totalCursor, list ) );
                }
                break;
            }
        } finally {
            this.totalCursor += nframes;
        }
    }
}
