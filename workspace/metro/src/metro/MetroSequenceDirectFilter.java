package metro;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.logging.Level;

import lamu.lib.log.Logger;

public class MetroSequenceDirectFilter implements MetroSequence, MetroLock {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }

    public MetroSequenceDirectFilter( int delay, MetroPort inputPort, MetroPort outputPort ) {
        this.delay = delay;
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
    private final Object lock = new Object();
    @Override
    public Object getMetroLock() {
        return lock;
    }
    
    int delay = -1;
    public int getRecordLength() {
        return delay;
    }
    public void setRecordLength(int delay) {
        this.delay = delay;
    }

    private LinkedList<MetroMidiEvent> buffer = new LinkedList<>();
    private LinkedList<MetroMidiEvent> workBuffer = new LinkedList<>();
    static void bufferDuplicate(List<MetroMidiEvent> buffer, List<MetroMidiEvent> obj) {
        for ( Iterator<MetroMidiEvent> i=obj.iterator(); i.hasNext(); ) {
            buffer.add( DefaultMetroMidiEvent.duplicate( i.next() ) );
        }
    }
    static void bufferReplacePort( Collection<MetroMidiEvent> buffer, MetroPort fromPort, MetroPort toPort ) {
        for ( Iterator<MetroMidiEvent> i=buffer.iterator(); i.hasNext(); ) {
            MetroMidiEvent e = i.next();
            if ( e.getPort().equals( fromPort )) {
//                logInfo("replace:" + e );
                e.setPort( toPort );
            }
        }
    }

    static void bufferInput( Collection<MetroMidiEvent> buffer, Collection<MetroMidiEvent> obj ) {
        for ( Iterator<MetroMidiEvent> i=obj.iterator(); i.hasNext(); ) {
            buffer.add( i.next() );
        }
    }
    static void bufferMove( Collection<MetroMidiEvent> buffer, int width ) {
        for ( Iterator<MetroMidiEvent> i=buffer.iterator(); i.hasNext(); ) {
            MetroMidiEvent e = i.next();
            e.moveMidiOffset( width );
            
            if ( e.getMidiOffset() < 0 ) {
//                logInfo("remove:" + e.getMidiOffset() );
                i.remove();
            } else {
//                logInfo("move:" + e.getMidiOffset() );
            }
        }
    }
    static void bufferOutput( Collection<MetroMidiEvent> buffer, Collection<MetroMidiEvent> obj, int rangeFrom, int rangeTo ) {
        boolean o=false;
        for ( Iterator<MetroMidiEvent> i=buffer.iterator(); i.hasNext(); ) {
            MetroMidiEvent e = i.next();
            int offset = e.getMidiOffset();
            if ( rangeFrom <= offset  && offset < rangeTo ) {
                logInfo("output:" + e );
                o = true;
                obj.add( DefaultMetroMidiEvent.duplicate(e) );
            }
        }
        if ( o)
            logInfo("===");
    }
    static Comparator<MetroMidiEvent> COMPARATOR = new Comparator<MetroMidiEvent>() {
        int noteon = MetroMidi.MIDI_NOTE_ON.getStatusHigher4bit();
        @Override
        public int compare(MetroMidiEvent o1, MetroMidiEvent o2) {
            int o0 = o1.getMidiOffset() - o2.getMidiOffset();
            if ( o0 != 0 ) { 
                return o0;
            } else {
                int ch1 = MetroMidi.getMidiChannel( o1.getMidiData() );
                int ch2 = MetroMidi.getMidiChannel( o2.getMidiData() );
                int ch0 = ch1-ch2;
                if ( ch0!=0 ) {
                    return ch0;
                } else {
                    int c1 = MetroMidi.getMidiCommand( o1.getMidiData() );
                    int c2 = MetroMidi.getMidiCommand( o2.getMidiData() );
                    boolean noteon1 = c1 == noteon;
                    boolean noteon2 = c2 == noteon;
                    if ( noteon1 == noteon2 ) {
                        return 0;
                    } else if ( noteon1 ) {
                        return -1;
                    } else if ( noteon2 ) {
                        return +1;
                    } else {
                        throw new Error( "internal error" );
                    }
                }

            }
        }
    };
    static void bufferSort( List<MetroMidiEvent> buffer ) {
        Collections.sort( buffer, COMPARATOR );
    }
    public static void main(String[] args) {
        ArrayList<MetroMidiEvent> list = new ArrayList<>();
        list.add( new DefaultMetroMidiEvent( 10, null, MetroMidi.MIDI_NOTE_OFF.createMidi(0, 65, 64 ) ) );
        list.add( new DefaultMetroMidiEvent( 10, null, MetroMidi.MIDI_NOTE_ON.createMidi(0, 65, 64 ) ) );
        list.add( new DefaultMetroMidiEvent( 10, null, MetroMidi.MIDI_NOTE_OFF.createMidi(1, 63, 64 ) ) );
        list.add( new DefaultMetroMidiEvent( 10, null, MetroMidi.MIDI_NOTE_ON.createMidi(1, 63, 64 ) ) );
        list.add( new DefaultMetroMidiEvent( 0, null, MetroMidi.MIDI_NOTE_OFF.createMidi(0, 65, 64 ) ) );
        list.add( new DefaultMetroMidiEvent( 0, null, MetroMidi.MIDI_NOTE_ON.createMidi(0, 65, 64 ) ) );
        list.add( new DefaultMetroMidiEvent( 0, null, MetroMidi.MIDI_NOTE_OFF.createMidi(1, 63, 64 ) ) );
        list.add( new DefaultMetroMidiEvent( 0, null, MetroMidi.MIDI_NOTE_ON.createMidi(1, 63, 64 ) ) );
        bufferSort(list);
        for ( MetroMidiEvent e : list ) {
            System.out.println( e );
        }
    }

    @Override
    public void processDirect(Metro metro, int nframes, int totalCursor, List<MetroMidiEvent> in, List<MetroMidiEvent> out) {
//        if ( true ) {
//            bufferReplacePort(in, inputPort , outputPort );
//            out.addAll(in);
//            bufferSort( out );
//            return;
//        }
//        logInfo( "filter:" + nframes);
        synchronized ( this.buffer ) {
            workBuffer.clear();
            bufferDuplicate(   workBuffer, in );
            bufferReplacePort( workBuffer, inputPort , outputPort );
            bufferMove(   workBuffer, delay );
            bufferInput ( buffer, workBuffer );
            bufferSort  ( buffer );
            bufferOutput( buffer, out, 0, nframes );
            bufferMove  ( buffer, nframes * -1 );
        }
        
//        switch ( this.mode ) {
//            case PLAY:
//                if ( player == null ) {
//                    player = frames.iterator();
//                }
//                
//                if ( ! player.hasNext() ) {
//                    if ( isLooper() ) {
//                        player = frames.iterator();
//                    }
//                }
//                
//                if ( ! player.hasNext() ) {
//                    out.addAll( player.next().getMidiEventList() );
//                }
//
//                break;
//            case RECORD:
//                if ( 0< this.recordLength && this.recordLength < totalCursor ) {
//                    this.setMode( Mode.PLAY );
//                } else {
//                    ArrayList<MetroMidiEvent> list = new ArrayList<>();
//                    for ( MetroMidiEvent e : in ) {
//                        if ( e.getPort().equals( this.inputPort ) ) {
//                            e.setPort( this.outputPort );
//                            list.add(e);
//                        }
//                    }
//                    synchronized ( getMetroLock() ) {
//                        frames.add( new MetroMidiFrame( totalCursor, list ) );
//                    }
//                }
//                break;
//        }
    }

    @Override
    public <T> void processBuffered( Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T> buffer ) {
    }
}
