package metro;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class MetroSequenceDirectRecorder implements MetroSequence, MetroLock {
    public MetroSequenceDirectRecorder( int recordLength, boolean looper, MetroPort inputPort, MetroPort outputPort ) {
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

    private final Object lock = new Object();
    @Override
    public Object getMetroLock() {
        return lock;
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

    @Override
    public void processDirect(Metro metro, int nframes, int totalCursor, List<MetroMidiEvent> in, List<MetroMidiEvent> out) {
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
                    out.addAll( player.next().getMidiEventList() );
                }

                break;
            case RECORD:
                if ( 0< this.recordLength && this.recordLength < totalCursor ) {
                    this.setMode( Mode.PLAY );
                } else {
                    ArrayList<MetroMidiEvent> list = new ArrayList<>();
                    for ( MetroMidiEvent e : in ) {
                        if ( e.getPort().equals( this.inputPort ) ) {
                            e.setPort( this.outputPort );
                            list.add(e);
                        }
                    }
                    synchronized ( getMetroLock() ) {
                        frames.add( new MetroMidiFrame( totalCursor, list ) );
                    }
                }
                break;
        }
    }

    @Override
    public void processBuffered( Metro metro, MetroTrack track, MetroEventBuffer buf ) {
    }
}
