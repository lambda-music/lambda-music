package metro;

import java.lang.invoke.MethodHandles;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.logging.Level;

import lamu.lib.logging.Logger;

/**
 * 
 * 
 * 
 */
public class MetroMidiOutputSignalAnalyzer {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }

    final Object lock = new Object();
    boolean resetFlag = false;
    MetroTrackMode trackMode = MetroTrackMode.POLY;
    public MetroTrackMode getTrackMode() {
        return trackMode;
    }
    public void setTrackMode(MetroTrackMode trackMode) {
        synchronized ( lock ) {
            if ( trackMode.equals( MetroTrackMode.MONO) && this.trackMode.equals(MetroTrackMode.POLY) ) {
                this.resetFlag = true;
            }
            this.trackMode = trackMode;
            switch ( trackMode ) {
            case MONO :
                this.receiver = enabledReceiver;
                break;
            case POLY :
                this.receiver = disabledReceiver;
                break;
            }
        }
    }

    abstract class NoteElement {
        private MetroPort port;
        private int channel;
        private int noteNumber;
        public NoteElement(MetroPort port, int channel, int noteNumber) {
            super();
            this.port = port;
            this.channel = channel;
            this.noteNumber = noteNumber;
        }
        public MetroPort getPort() {
            return port;
        }
        public int getChannel() {
            return channel;
        }
        public int getNoteNumber() {
            return noteNumber;
        }
        public abstract void getNoteOn(MetroCollector<byte[]> result);
        public abstract void getNoteOff(MetroCollector<byte[]> result);
    }

    class DoubleNoteElement extends NoteElement {
        double velocity;
        public DoubleNoteElement(MetroPort port, int channel, int noteNumber, double velocity ) {
            super(port, channel, noteNumber);
            this.velocity = velocity;
        }
        @Override
        public void getNoteOn(MetroCollector<byte[]> result) {
            MetroMidi.MIDI_NOTE_ON.createMidi(result, getChannel(), getNoteNumber(), velocity );
        }
        @Override
        public void getNoteOff(MetroCollector<byte[]> result) {
            MetroMidi.MIDI_NOTE_OFF.createMidi(result, getChannel(), getNoteNumber(), 0 );
        }
        public double getVelocity() {
            return velocity;
        }
    }

    private final Deque<NoteElement> noteStack = new ArrayDeque<NoteElement>();
    void noteOnProc( MetroCollector<byte[]> result, int channel, int note, double velocity ) {
        synchronized ( lock ) {
            if ( noteStack.isEmpty() ) {
                DoubleNoteElement noteElement = new DoubleNoteElement( currentEvent.getPort(), channel, note, velocity );
                noteStack.push( noteElement );
                noteElement.getNoteOn( result );
            } else {
                noteStack.peekFirst().getNoteOff(result);
                DoubleNoteElement noteElement = new DoubleNoteElement( currentEvent.getPort(), channel, note, velocity );
                noteStack.push( noteElement );
                noteElement.getNoteOn(result);
            }
        }
    }

    void noteOffProc( MetroCollector<byte[]> result, int channel, int note, double velocity ) {
        synchronized ( lock ) {
            if ( noteStack.isEmpty() ) {
                // There supposed not to be note-off signal here; therefore, this will not
                // happen in general case. In case it happened as such when the buffer
                // underflowed, ignore the note-off signal.
            } else {

                // A flag which is true when the iterator is on the first row.
                boolean first  =  true;

                // A flag which becomes true after the first only if the first row matched to
                // the specified note number and channel.
                boolean second = false;

                // loop
                for ( Iterator<NoteElement> i = noteStack.iterator(); i.hasNext(); ) {
                    NoteElement noteElement = i.next();

                    // CHECK-2 Revive the second note; see CHECK-1
                    if ( second ) {
                        second = false;
                        // Revive the muted second note.
                        noteElement.getNoteOn(result);
                    } else {
                        // CHECK-1 Remove the matched note. 
                        if ( noteElement.channel == channel && noteElement.noteNumber == note ) {
                            i.remove();

                            // - Set `second` flag if it is on the first row.
                            // - This causes the note on the next row to be revived.
                            // - Note that `second` flag will be ignored if it is on the last row; that is,
                            //   there is only one row.
                            if ( first ) {
                                second = true;
                                noteElement.getNoteOff(result);
                            }
                        }                   
                    }

                    // Reset the `first` flag.
                    first = false;
                }
            }
        }
    }

    List<byte[]> disabledNoteOnProc( MetroCollector<byte[]> result, int channel, int note, double velocity ) {
        synchronized ( lock ) {
            return null;
        }
    }

    List<byte[]> disabledNoteOffProc( MetroCollector<byte[]> result, int channel, int note, double velocity ) {
        synchronized ( lock ) {
            return null;
            //            if ( noteStack.isEmpty() ) {
            //                // There supposed not to be note-off signal here; therefore, this will not
            //                // happen in general case. In case it happened as such when the buffer
            //                // underflowed, ignore the note-off signal.
            //                return null;
            //            } else {
            //                boolean found = false;
            //                // loop
            //                for ( Iterator<NoteElement> i = noteStack.iterator(); i.hasNext(); ) {
            //                    NoteElement noteElement = i.next();
            //
            //                    // CHECK-1 Remove the matched note. 
            //                    if ( noteElement.channel == channel && noteElement.noteNumber == note ) {
            //                        i.remove();
            //                        found = true;
            //                    }                   
            //                }
            //                if ( found ) {
            //                    // Remove the current note.
            //                    return listPool.withdraw();
            //                } else {
            //                    // Leave the current note.
            //                    return null;
            //                }
            //            }
        }
    }


    final class CollectorPool<T> extends Pool<MetroSignalAnalyzerCollector<T>> {
        CollectorPool(int initialSize) {
            super(initialSize);
        }
        @Override
        protected MetroSignalAnalyzerCollector<T> create() {
            return new MetroSignalAnalyzerCollector<T>();
        }
        @Override
        protected MetroSignalAnalyzerCollector<T> initializeValue(MetroSignalAnalyzerCollector<T> o) {
            o.reset();
            return o;
        }
    }
    private final Pool<MetroSignalAnalyzerCollector<byte[]>> listPool = new CollectorPool<byte[]>(2);


    private final class EnabledReceiver extends DefaultMetroMidiReceiver<byte[]> {
        @Override
        public void noteOn(MetroCollector<byte[]> result, int channel, int note, double velocity) {
            MetroSignalAnalyzerCollector.setReplaced(result,true);
            noteOnProc(result, channel,note,velocity );
        }
        @Override
        public void noteOff(MetroCollector<byte[]> result,int channel, int note, double velocity) {
            MetroSignalAnalyzerCollector.setReplaced(result,true);
            noteOffProc(result, channel,note,velocity);
        }

        @Override
        public void noteOn(MetroCollector<byte[]> result,int channel, int note, int velocity) {
            MetroSignalAnalyzerCollector.setReplaced(result,true);
        }
        @Override
        public void noteOff(MetroCollector<byte[]> result,int channel, int note, int velocity) {
            MetroSignalAnalyzerCollector.setReplaced(result,true);
            super.noteOff(result,channel, note, velocity);
        }
    }
    private MetroMidiReceiver<byte[]> enabledReceiver = new MetroMidiOutputSignalAnalyzer.EnabledReceiver();

    private final class DisabledReceiver extends MetroMidiReceiver.Default<byte[]> {
        @Override
        protected byte[] defaultValue() {
            return null;
        }
        @Override
        public void noteOn(MetroCollector<byte[]> result,int channel, int note, double velocity) {
            disabledNoteOnProc(result, channel,note,velocity);
        }
        @Override
        public void noteOff(MetroCollector<byte[]> result,int channel, int note, double velocity) {
            disabledNoteOffProc(result, channel,note,velocity);
        }

        //        @Override
        //        public byte[] noteOn(int channel, int note, int velocity) {
        //            byte[] list = listPool.withdraw();
        //            return list;
        ////          return super.noteOn(channel, note, velocity);
        //        }
        @Override
        public void noteOff(MetroCollector<byte[]> result, int channel, int note, int velocity) {
            super.noteOff(result, channel, note, velocity);
        }
    }
    private MetroMidiReceiver<byte[]> disabledReceiver = new MetroMidiOutputSignalAnalyzer.DisabledReceiver();
    private MetroMidiReceiver<byte[]> receiver = disabledReceiver;
    
    static class MetroSignalAnalyzerCollector<T> implements MetroCollector<T> {
        static <T> void setReplaced( MetroCollector<T> collector, boolean value ) {
            ((MetroSignalAnalyzerCollector<byte[]>)collector).setReplaced(true);
        }
        private boolean replaced = false;
        public boolean isReplaced() {
            return replaced;
        }
        public void setReplaced(boolean replaced) {
            this.replaced = replaced;
        }
        private final List<T> result = new ArrayList<T>();
        public List<T> getResult() {
            return result;
        }
        @Override
        public void collect(T value) {
            this.result.add( value );
        }
        public void reset() {
            this.result.clear();
            this.replaced = false;
        }
    }

    volatile MetroMidiEvent currentEvent = null;
    public void process( List<MetroMidiEvent> outputMidiEvents ) {
        synchronized (lock) {
            this.currentEvent = null;

            MetroSignalAnalyzerCollector<byte[]> result = listPool.withdraw();
            try {
                for ( ListIterator<MetroMidiEvent> i=outputMidiEvents.listIterator();i.hasNext();  ) {
                    MetroMidiEvent e = i.next();
                    this.currentEvent = e;
                    
                    // Invoke the analyzing receiver.
                    result.reset();
                    MetroMidi.receiveMidiMessage(result, receiver, e );
                    
                    // If it returns `null`, do nothing; otherwise replace the current element with
                    // the elements in the list. If it returns an empty list, this routine
                    // effectively remove the current element. See Receiver.
                    
                    // MODIFIED (Fri, 07 Aug 2020 11:37:52 +0900)
                    // If isReplaced() flag is on, replace the current element with the returned elements;
                    // otherwise, leave the current element as it is.
                    
                    if ( result.isReplaced() ) {
                        boolean first = true;
                        List<byte[]> midiDataList = result.getResult();
                        if ( midiDataList.isEmpty() ) {
                            i.remove();
                        } else {
                            for ( byte[] midiData : midiDataList ) {
                                if ( first ) {
                                    first = false;
                                    e.setMidiData( midiData );
                                } else {
                                    i.add( new DefaultMetroMidiEvent(e.getMidiOffset(), e.getPort(), midiData ) );
                                }
                            }
                        }
                    } else {
                        // do nothing
                    }
                }

                // disabled (Mon, 03 Aug 2020 09:24:55 +0900)
                if ( false ) {
                    if ( this.resetFlag ) {
                        this.resetFlag = false;
                        if ( noteStack.isEmpty() ) {
                        } else {
                            if ( outputMidiEvents.isEmpty() ) {
                                NoteElement noteElement = noteStack.peekFirst();
                                result.reset();
                                noteElement.getNoteOff(result);
                                if ( ! result.getResult().isEmpty() )
                                    outputMidiEvents.add( new DefaultMetroMidiEvent(0, noteElement.getPort(), result.getResult().get(0) ) );
                                else
                                    logError( "WARNING!! result was empty", new Error() );
                            } else {
                                MetroMidiEvent lastEvent = outputMidiEvents.get(outputMidiEvents.size()-1);
                                NoteElement noteElement = noteStack.peekFirst();
                                result.reset();
                                noteElement.getNoteOff(result);
                                if ( ! result.getResult().isEmpty() )
                                    outputMidiEvents.add( new DefaultMetroMidiEvent(lastEvent.getMidiOffset(), noteElement.getPort(), result.getResult().get(0) ) );
                                else
                                    logError( "WARNING!! result was empty", new Error() );
                            }
                        }
                    }
                }
            } finally {
                listPool.deposit(result);
            }


        }
    }
}
