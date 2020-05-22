package metro;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.logging.Level;

import lamu.lib.log.Logger;

public class MetroDirectFilterSequence implements MetroSequence {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }
    
    public static void test( int[] aaa ) {
        System.out.println( "hello:" );
        for ( int i: aaa) {
            System.out.println( i);
        }
    }
    
    public MetroDirectFilterSequence( int delay, int threshold, int[] velocityMap, MetroPort inputPort, MetroPort outputPort ) {
        if ( delay < 0 )
            throw new IllegalArgumentException("`delay` cannot be less than zero" );
        if ( threshold < 0 )
            throw new IllegalArgumentException("`threshold` cannot be less than zero" );
        if ( velocityMap == null )
            throw new IllegalArgumentException("`velocityMap` cannot be null" );
        if ( velocityMap.length < 128 )
            throw new IllegalArgumentException("the size of `velocityMap` must be greater than or equal to 128" );
        
        this.delay = delay;
        this.threshold = threshold;
        this.velocityMap = velocityMap;
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
    
    
    int delay = -1;
    public int getDelay() {
        return delay;
    }
    public void setDelay(int delay) {
        this.delay = delay;
    }

    int[] velocityMap;
    public int[] getVelocityMap() {
        return velocityMap;
    }

    int threshold = 100;
    public int getThreshold() {
        return threshold;
    }
    public void setThreshold(int threshold) {
        this.threshold = threshold;
    }
    
    private List<MetroMidiEvent> buffer = new ArrayList<>();
    private List<MetroMidiEvent> workBuffer = new ArrayList<>();
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
    static void bufferMove( Collection<MetroMidiEvent> buffer, long width ) {
        for ( Iterator<MetroMidiEvent> i=buffer.iterator(); i.hasNext(); ) {
            MetroMidiEvent e = i.next();
            e.moveMidiOffset( width );
            if ( e.getMidiOffset() < 0 ) {
                i.remove();
            }
        }
    }
    static void bufferOutput( Collection<MetroMidiEvent> buffer, Collection<MetroMidiEvent> obj, long rangeFrom, long rangeTo ) {
        for ( Iterator<MetroMidiEvent> i=buffer.iterator(); i.hasNext(); ) {
            MetroMidiEvent e = i.next();
            long offset = e.getMidiOffset();
            if ( rangeFrom <= offset  && offset <= rangeTo ) {
                obj.add( DefaultMetroMidiEvent.duplicate(e) );
            }
        }
    }
    static Comparator<MetroMidiEvent> COMPARATOR = new Comparator<MetroMidiEvent>() {
        int noteon = MetroMidi.MIDI_NOTE_ON.getStatusHigher4bit();
        @Override
        public int compare(MetroMidiEvent o1, MetroMidiEvent o2) {
            long o0 = o1.getMidiOffset() - o2.getMidiOffset();
            if ( o0 != 0 ) { 
                return (int) o0;
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
    
    public void bufferPreProcess( List<MetroMidiEvent> in  ) {
        bufferMove( workBuffer, delay );
    }
    public void bufferPostProcess(List<MetroMidiEvent> out) {
        for ( MetroMidiEvent e : out ) {
            int midiCommand = e.getMidiCommand();
            if ( midiCommand == noteOn || midiCommand == noteOff ) {
                byte[] midiData = e.getMidiData();
                midiData[2] = (byte)velocityMap[ MetroMidi.MASK_7BIT & midiData[2] ]; 
            }
        }
    }
    private static int noteOn = MetroMidi.MIDI_NOTE_ON.statusHigher4bit;
    private static int noteOff = MetroMidi.MIDI_NOTE_OFF.statusHigher4bit;
    private volatile MetroMidiEvent[] noteArray = new MetroMidiEvent[128];
    private volatile ArrayList<MetroMidiEvent> removingNoteList = new ArrayList<>(1024);
    public synchronized void bufferProcess( List<MetroMidiEvent> buffer ) {
        java.util.Arrays.fill(noteArray, null);
        removingNoteList.clear();
        for (ListIterator<MetroMidiEvent> i = buffer.listIterator(buffer.size()); i.hasPrevious(); ) {
            MetroMidiEvent curr = i.previous();
            if ( curr.getMidiCommand() == noteOff ) {
                noteArray[ MetroMidi.getMidiNoteNumber(curr.getMidiData()) ] = curr;
            } else if ( curr.getMidiCommand() == noteOn ) {
                int noteOffIdx = MetroMidi.getMidiNoteNumber(curr.getMidiData());
                MetroMidiEvent noteOff = noteArray[ noteOffIdx ];
                if ( noteOff != null ) {
                    if ((noteOff.getMidiOffset() - curr.getMidiOffset()) < threshold ) {
                        removingNoteList.add( noteOff );
                        removingNoteList.add( curr );
                        noteArray[noteOffIdx] = null;
                    }
                }
            }
        }
        buffer.removeAll( removingNoteList );
    }

    @Override
    public void progressBuffer(Metro metro, MetroTrack track, long measureLengthInFrames) throws MetroException {
    }
    @Override
    public void progressCursor(
        Metro metro, 
        MetroTrack track, 
        long nframes,
        long measureLengthInFrames, List<MetroMidiEvent> inputMidiEvents, List<MetroMidiEvent> outputMidiEvents, List<MetroTrack> tracks, List<MetroTrack> registeringTracks, List<MetroTrack> unregisteringTracks ) throws MetroException 
    {
//        if ( true ) {
//            bufferReplacePort(in, inputPort , outputPort );
//            out.addAll(in);
//            bufferSort( out );
//            return;
//        }
//        logInfo( "filter:" + nframes);


        synchronized ( this ) {
            workBuffer.clear();
            bufferDuplicate(   workBuffer, inputMidiEvents );
            bufferReplacePort( workBuffer, inputPort , outputPort );
            bufferPreProcess( workBuffer );
            bufferInput ( buffer, workBuffer );
            bufferProcess( buffer );
            bufferOutput( buffer, outputMidiEvents, 0, nframes );
            bufferPostProcess( outputMidiEvents );
            bufferMove  ( buffer, nframes * -1 );
        }
        
        /* 
         * NOTE : (Fri, 17 Apr 2020 01:32:44 +0900) 
         * Actually sorting is very harmful by a reason; it is commented out.
         * The reason is that it is not suffice to let note-on come before note-off.
         * Consider the following situation:
         * ----
         * 0:00 C4 NOTE ON
         * -----
         * 0:01 C4 NOTE OFF
         * 0:02 C4 NOTE ON
         * ----
         * 
         * in the above example, placing the note on before note off causes a problem that
         * some notes which are not stopping at the note-off event in the client synth.
         * It's the best not to sort them. 
         * 
         * bufferSort  ( buffer );
         */
    }
}
