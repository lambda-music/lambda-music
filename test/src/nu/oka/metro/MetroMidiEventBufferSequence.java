package nu.oka.metro;

import java.util.Iterator;
import java.util.List;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import org.jaudiolibs.jnajack.JackClient;
import org.jaudiolibs.jnajack.JackException;
import org.jaudiolibs.jnajack.JackPosition;

class MetroMidiEventBufferSequence {
	/**
	 * 
	 */
	private final Metro metro;

	@SuppressWarnings("unused")
	private static final boolean DEBUG = false;

	// static final int BUFFER_SIZE = 2;
	static final int BUFFER_SIZE = 2;

	int id = (int) (Math.random()* Integer.MAX_VALUE);

	private MetroLogicHandle handle = new MetroLogicHandle() {
		@Override
		public void spawn( double offset, MetroLogic logic ) {
			MetroMidiEventBufferSequence sequence = 
					new MetroMidiEventBufferSequence( metro, MetroMidiEventBufferSequence.this, logic, offset );
			
			metro.registerSequence( sequence );
		}
	};

	private MetroMidiEventBufferSequence parentSequence;
	private BlockingQueue<MetroMidiEventBuffer> buffers = new LinkedBlockingQueue<>();
	private int cursor = 0;
	private MetroLogic logic;
	private double offset=0.0d;

	public MetroMidiEventBufferSequence( Metro metro, MetroMidiEventBufferSequence parent, MetroLogic logic, double offset ) {
		this.metro = metro;
		this.parentSequence = parent;
		this.logic = logic;
		this.offset = offset;
		
		this.logic.setLogicHandle( handle );
	}
	public MetroLogic getLogic() {
		return this.logic;
	}


	public void progressCursor( int nframes, List<MetroMatchedEvent> result ) throws JackException {
		this.metro.clearAllPorts();

		int nextCursor = this.cursor + nframes;
		
		// This keeps negative offset value for the current cursor position. 
		int cursorOffset = 0;

		//    	System.out.println( cursor + "/" + nextCursor );
		int accrossCount = 0;
		
		for ( Iterator<MetroMidiEventBuffer> ibuf = this.buffers.iterator(); ibuf.hasNext(); ) {
			MetroMidiEventBuffer buf = ibuf.next();

			int actualCursor = this.cursor - cursorOffset;
			int actualNextCursor = nextCursor - cursorOffset;

			//    		System.out.println( "AFTER::::" );
			//    		buf.dump();
			for ( Iterator<MetroMidiEvent> ie = buf.iterator(); ie.hasNext();  ) {
				MetroMidiEvent e = ie.next();

				
				if ( e.between( actualCursor, actualNextCursor ) ) {
					//		    			System.out.println("VALUE" + ( e.getOffsetInFrames() - this.cursor ) );
					//        			System.out.println( e.("***" ));

					result.add( new MetroMatchedEvent( 
							e.getOutputPortNo(), 
							e.getOffsetInFrames() - actualCursor, 
							e.getData() 
							) );

					//        			System.out.println( "event.getData()" + event.getData() );
					//        			System.out.println( "event.getData()" + Integer.toUnsignedString(event.getData()[0] , 2 ) );
				}
			}

			// System.out.println( buf.getLengthInFrames() );

//			/* 
//			 * (Sat, 23 Dec 2017 23:16:25 +0900)
//			 * 1. 
//			 * This "if" statement should not be like :
//			 *     buf.getLengthInFrames() <= nextCursor
//			 * This should be :
//			 *     buf.getLengthInFrames() <   nextCursor
//			 *  
//			 * Because there might be a note on the last position.
//			 * This may often be a note off event.
//			 * 
//			 * 2.
//			 *    this.cursor= this.cursor - buf.getLengthInFrames();
//			 *    
//			 * In this statement `this.cursor` could be lower than zero.
//			 * This value is used when when we call JackMidi.eventWrite() afterwards 
//			 * as offset value of the current frame.  
//			 * 
//			 */
//			if ( buf.getLengthInFrames() <   actualNextCursor  ) {
//				// if ( DEBUG ) System.out.println( "1 cursor=" + this.cursor  + "/nextCursor=" + nextCursor  + " buf.getLengthInFrames=" + buf.getLengthInFrames());
////				nextCursor =  nextCursor - buf.getLengthInFrames();
////				this.cursor= this.cursor - buf.getLengthInFrames() ;
//				cursorOffset = cursorOffset + buf.getLengthInFrames();
//				// if ( DEBUG ) System.out.println( "2 cursor=" + this.cursor  + "/nextCursor=" + nextCursor );
//				// accrossCount ++;
//				//        		break;
//			} else {
//				break;
//			}
			cursorOffset = cursorOffset + buf.getLengthInFrames();
//			System.out.println( cursorOffset );
		}
		//		for ( int i=0; i< accrossCount; i++ )
		//		this.buffers.poll();
		
		for (;;) {
			if ( this.buffers.isEmpty() )
				break;
			int lengthInFrames = this.buffers.peek().getLengthInFrames();
			
			if (  lengthInFrames < this.cursor ) {
				this.cursor -= lengthInFrames;
				nextCursor -= lengthInFrames;
				this.buffers.poll();
			} else {
				break;
			}
		}


		this.cursor = nextCursor;
		if ( Metro.DEBUG )
			System.out.println( this.cursor + "/" + (this.buffers.isEmpty() ? "empty" : this.buffers.peek().getLengthInFrames()  ) );
	}

	public void prepare( int barInFrames ) throws JackException {
		int parentCursor = this.parentSequence.cursor;
		this.cursor = parentCursor + (int) (-1.0d * this.offset * barInFrames) ;
	}

	public void reprepare( Metro metro, JackClient client, JackPosition position ) throws JackException {
		for ( MetroMidiEventBuffer buffer : this.buffers ) {
			buffer.prepare(metro, client, position);
		}
	}

	public void checkBuffer( Metro metro, JackClient client, JackPosition position ) throws JackException {
		if ( this.buffers.size() < BUFFER_SIZE ) {
			this.offerNewBuffer( metro, client, position );
		}
	}

	private void offerNewBuffer( Metro metro, JackClient client, JackPosition position ) throws JackException {
		MetroMidiEventBuffer buf = new MetroMidiEventBuffer();
		boolean result = this.logic.processBuffer( buf );

		buf.prepare( metro, client, position );

		if ( result ) {
			this.buffers.offer(buf);
		} else {
			metro.unregisterSequence( this );
		}
		// buf.dump();
	}
} 