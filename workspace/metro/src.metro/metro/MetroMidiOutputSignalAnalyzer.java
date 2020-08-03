package metro;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;

/**
 * 
 * 
 * 
 */
public class MetroMidiOutputSignalAnalyzer {
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
		public abstract byte[] getNoteOn();
		public abstract byte[] getNoteOff();
	}

	class DoubleNoteElement extends NoteElement {
		double velocity;
		public DoubleNoteElement(MetroPort port, int channel, int noteNumber, double velocity ) {
			super(port, channel, noteNumber);
			this.velocity = velocity;
		}
		@Override
		public byte[] getNoteOn() {
			return MetroMidi.MIDI_NOTE_ON.createMidi( getChannel(), getNoteNumber(), velocity );
		}
		@Override
		public byte[] getNoteOff() {
			return MetroMidi.MIDI_NOTE_OFF.createMidi( getChannel(), getNoteNumber(), 0 );
		}
		public double getVelocity() {
			return velocity;
		}
	}
	
	private final Deque<NoteElement> noteStack = new ArrayDeque<NoteElement>();
	List<byte[]> noteOnProc( int channel, int note, double velocity ) {
		synchronized ( lock ) {
			List<byte[]> list = listPool.withdraw();
			if ( noteStack.isEmpty() ) {
				DoubleNoteElement noteElement = new DoubleNoteElement( currentEvent.getPort(), channel, note, velocity );
				noteStack.push( noteElement );
				list.add( noteElement.getNoteOn() );
			} else {
				list.add( noteStack.peekFirst().getNoteOff() );
				DoubleNoteElement noteElement = new DoubleNoteElement( currentEvent.getPort(), channel, note, velocity );
				noteStack.push( noteElement );
				list.add( noteElement.getNoteOn() );
			}
			return list;
		}
	}
	
    List<byte[]> noteOffProc( int channel, int note, double velocity ) {
        synchronized ( lock ) {
            List<byte[]> list = listPool.withdraw();
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
                        list.add( noteElement.getNoteOn() );
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
                                list.add( noteElement.getNoteOff() );
                            }
                        }                   
                    }

                    // Reset the `first` flag.
                    first = false;
                }
            }
            return list;
        }
    }

    List<byte[]> disabledNoteOnProc( int channel, int note, double velocity ) {
        synchronized ( lock ) {
            return null;
        }
    }

    List<byte[]> disabledNoteOffProc( int channel, int note, double velocity ) {
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

	
	final class ArrayListPool extends Pool<List<byte[]>> {
		ArrayListPool(int initialSize) {
			super(initialSize);
		}
		@Override
		protected ArrayList<byte[]> create() {
			return new ArrayList<byte[]>();
		}
		@Override
		protected List<byte[]> initializeValue(List<byte[]> o) {
			o.clear();
			return o;
		}
	}
	private final Pool<List<byte[]>> listPool = new ArrayListPool(2);
	
	
	private final class EnabledReceiver extends MetroMidiReceiver.Default<List<byte[]>> {
		@Override
		protected List<byte[]> defaultValue() {
			return null;
		}
		@Override
		public List<byte[]> noteOn(int channel, int note, double velocity) {
			return noteOnProc(channel,note,velocity);
		}
		@Override
		public List<byte[]> noteOff(int channel, int note, double velocity) {
			return noteOffProc(channel,note,velocity);
		}

		@Override
		public List<byte[]> noteOn(int channel, int note, int velocity) {
			List<byte[]> list = listPool.withdraw();
			return list;
//			return super.noteOn(channel, note, velocity);
		}
		@Override
		public List<byte[]> noteOff(int channel, int note, int velocity) {
			return super.noteOff(channel, note, velocity);
		}
	}
    private MetroMidiReceiver<List<byte[]>> enabledReceiver = new MetroMidiOutputSignalAnalyzer.EnabledReceiver();

    private final class DisabledReceiver extends MetroMidiReceiver.Default<List<byte[]>> {
        @Override
        protected List<byte[]> defaultValue() {
            return null;
        }
        @Override
        public List<byte[]> noteOn(int channel, int note, double velocity) {
            return disabledNoteOnProc(channel,note,velocity);
        }
        @Override
        public List<byte[]> noteOff(int channel, int note, double velocity) {
            return disabledNoteOffProc(channel,note,velocity);
        }

//        @Override
//        public List<byte[]> noteOn(int channel, int note, int velocity) {
//            List<byte[]> list = listPool.withdraw();
//            return list;
////          return super.noteOn(channel, note, velocity);
//        }
        @Override
        public List<byte[]> noteOff(int channel, int note, int velocity) {
            return super.noteOff(channel, note, velocity);
        }
    }
    private MetroMidiReceiver<List<byte[]>> disabledReceiver = new MetroMidiOutputSignalAnalyzer.DisabledReceiver();

    private MetroMidiReceiver<List<byte[]>> receiver = disabledReceiver;

    volatile MetroMidiEvent currentEvent = null;
	public void process( List<MetroMidiEvent> outputMidiEvents ) {
	    synchronized (lock) {
	        this.currentEvent = null;
	            
	        for ( ListIterator<MetroMidiEvent> i=outputMidiEvents.listIterator();i.hasNext();  ) {
	            MetroMidiEvent e = i.next();
	            this.currentEvent = e;
	            
	            // Invoke the analyzing receiver.
	            List<byte[]> midiDataList = MetroMidi.receiveMidiMessage( receiver, e );
	            
	            // If it returns `null`, do nothing; otherwise replace the current element with
	            // the elements in the list. If it returns an empty list, this routine
	            // effectively remove the current element. See Receiver.
	            if ( midiDataList != null ) {
	                boolean first = true;
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
	                listPool.deposit(midiDataList);
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
	                        outputMidiEvents.add( new DefaultMetroMidiEvent(0, noteElement.getPort(), noteElement.getNoteOff() ) );
	                    } else {
	                        MetroMidiEvent lastEvent = outputMidiEvents.get(outputMidiEvents.size()-1);
	                        NoteElement noteElement = noteStack.peekFirst();
	                        outputMidiEvents.add( new DefaultMetroMidiEvent(lastEvent.getMidiOffset(), noteElement.getPort(), noteElement.getNoteOff() ) );
	                    }
	                }
	            }
	        }
	    }
	}
}
