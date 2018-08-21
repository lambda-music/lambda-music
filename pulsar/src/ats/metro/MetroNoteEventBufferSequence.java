package ats.metro;

import java.util.Iterator;
import java.util.List;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.jaudiolibs.jnajack.JackClient;
import org.jaudiolibs.jnajack.JackException;
import org.jaudiolibs.jnajack.JackPosition;

public class MetroNoteEventBufferSequence {
	
	public enum SyncType {
		IMMEDIATE, PARALLEL, SERIAL,  
	}
	/**
	 * 
	 */
	protected final String name;
	private final Metro metro;

	@SuppressWarnings("unused")
	private static final boolean DEBUG = false;

	// static final int BUFFER_SIZE = 2;
	private static final int BUFFER_SIZE = 2;

	int id = (int) (Math.random()* Integer.MAX_VALUE);

	private MetroLogicHandle handle = new MetroLogicHandle() {
		@Override
		public void spawn( String name, double offset, MetroLogic logic ) {
			MetroNoteEventBufferSequence sequence = 
					new MetroNoteEventBufferSequence( metro, name, logic, syncType, MetroNoteEventBufferSequence.this, offset );
			
			metro.registerSequence( sequence );
		}
	};

	private SyncType syncType;
	private MetroNoteEventBufferSequence syncSequence;
	private double syncOffset=0.0d;
	private BlockingQueue<MetroNoteEventBuffer> buffers = new LinkedBlockingQueue<>();
	protected int cursor = 0;
	protected MetroLogic logic;
	
	public MetroNoteEventBufferSequence( Metro metro, String name, MetroLogic logic, SyncType syncType, MetroNoteEventBufferSequence syncSequence, double syncOffset ) {
		this.name = name.intern();
		this.metro = metro;
		this.logic = logic;
		this.syncType = syncType;
		this.syncSequence = syncSequence;
		this.syncOffset = syncOffset;
		
//		System.err.println(/ "" );
		
		this.logic.setLogicHandle( handle );
	}
	
	public MetroLogic getLogic() {
		return this.logic;
	}
	public String getName() {
		return name;
	}
	
//	@Override
//	public boolean equals(Object obj) {
//		try {
//			return this.name == ((MetroNoteEventBufferSequence)obj).name;
//		} catch ( ClassCastException e ) {
//            Logger.getLogger( Metro.class.getName()).log(Level.WARNING, null, e );
//			return false;
//		}
//	}

// ???
//	public int getCursor() {
//		return cursor;
//	}
//	public void setCursor( int cursor ) {
//		this.cursor = cursor; 
//	}


	protected void progressCursor( int nframes, List<MetroMidiEvent> result ) throws JackException {
		synchronized ( this.buffers ) {
			this.metro.clearAllPorts();

			int nextCursor = this.cursor + nframes;
			
			// This keeps negative offset value for the current cursor position. 
			int cursorOffset = 0;

			//    	System.out.println( cursor + "/" + nextCursor );
//			int accrossCount = 0;
			
			for ( Iterator<MetroNoteEventBuffer> ibuf = this.buffers.iterator(); ibuf.hasNext(); ) {
				MetroNoteEventBuffer buf = ibuf.next();

				int actualCursor = this.cursor - cursorOffset;
				int actualNextCursor = nextCursor - cursorOffset;

				//    		System.out.println( "AFTER::::" );
				//    		buf.dump();
				for ( Iterator<MetroNoteEvent> ie = buf.iterator(); ie.hasNext();  ) {
					MetroNoteEvent e = ie.next();

					
					if ( e.between( actualCursor, actualNextCursor ) ) {
						//		    			System.out.println("VALUE" + ( e.getOffsetInFrames() - this.cursor ) );
						//        			System.out.println( e.("***" ));

						result.add( new MetroMidiEvent( 
								e.getOutputPortNo(), 
								e.getOffsetInFrames() - actualCursor, 
								e.getData() 
								) );

						//        			System.out.println( "event.getData()" + event.getData() );
						//        			System.out.println( "event.getData()" + Integer.toUnsignedString(event.getData()[0] , 2 ) );
					}
				}

				// System.out.println( buf.getLengthInFrames() );

//				/* 
//				 * (Sat, 23 Dec 2017 23:16:25 +0900)
//				 * 1. 
//				 * This "if" statement should not be like :
//				 *     buf.getLengthInFrames() <= nextCursor
//				 * This should be :
//				 *     buf.getLengthInFrames() <   nextCursor
//				 *  
//				 * Because there might be a note on the last position.
//				 * This may often be a note off event.
//				 * 
//				 * 2.
//				 *    this.cursor= this.cursor - buf.getLengthInFrames();
//				 *    
//				 * In this statement `this.cursor` could be lower than zero.
//				 * This value is used when when we call JackMidi.eventWrite() afterwards 
//				 * as offset value of the current frame.  
//				 * 
//				 */
//				if ( buf.getLengthInFrames() <   actualNextCursor  ) {
//					// if ( DEBUG ) System.out.println( "1 cursor=" + this.cursor  + "/nextCursor=" + nextCursor  + " buf.getLengthInFrames=" + buf.getLengthInFrames());
////					nextCursor =  nextCursor - buf.getLengthInFrames();
////					this.cursor= this.cursor - buf.getLengthInFrames() ;
//					cursorOffset = cursorOffset + buf.getLengthInFrames();
//					// if ( DEBUG ) System.out.println( "2 cursor=" + this.cursor  + "/nextCursor=" + nextCursor );
//					// accrossCount ++;
//					//        		break;
//				} else {
//					break;
//				}
				cursorOffset = cursorOffset + buf.getLengthInFrames();
//				System.out.println( cursorOffset );
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
					metro.notifyCheckBuffer();
				} else {
					break;
				}
			}


			this.cursor = nextCursor;
			if ( Metro.DEBUG )
				System.out.println( this.cursor + "/" + (this.buffers.isEmpty() ? "empty" : this.buffers.peek().getLengthInFrames()  ) );
		}
	}

	protected void prepare( int barInFrames ) throws JackException {
		int offset = (int) (-1.0d * this.syncOffset * barInFrames);

		switch ( this.syncType ) {
			case IMMEDIATE :
			{
				this.cursor = offset;
				if ( this.syncSequence != null ) {
					Logger.getLogger( Metro.class.getName()).log(Level.WARNING, "syncSequence was passed but ignored by the process because syncType was `immediate`." );
				}
			}
			break;
			case PARALLEL :
			{
				if ( this.syncSequence == null ) {
					this.cursor = offset;
					Logger.getLogger( Metro.class.getName()).log(Level.WARNING, "`parallel` was specified but syncSequence was not passed." );
				} else {
					this.cursor = this.syncSequence.cursor + offset ;
				}

			}
			break;
			case SERIAL :
				if ( this.syncSequence == null ) {
					this.cursor = offset;
		            Logger.getLogger( Metro.class.getName()).log(Level.WARNING, "`serial` was specified but syncSequence was not passed." );
				} else {
					synchronized ( this.syncSequence.buffers ) {
						this.cursor =
								this.syncSequence.cursor - 
								this.syncSequence.buffers.peek().getLengthInFrames() + 
								offset;
					}
				}
				break;
			default :
				throw new RuntimeException( "Internal Error"); // this won't occur.
		}

	}

	protected  void reprepare( Metro metro, JackClient client, JackPosition position ) throws JackException {
		for ( MetroNoteEventBuffer buffer : this.buffers ) {
			buffer.prepare(metro, client, position);
		}
	}

	protected  void checkBuffer( Metro metro, JackClient client, JackPosition position ) throws JackException {
		if ( this.buffers.size() < BUFFER_SIZE ) {
			this.offerNewBuffer( metro, client, position );
		}
	}
	
	protected  void clearBuffer() {
		synchronized ( this.buffers ) {
			this.buffers.clear();
			this.cursor =0;
		}
	}
//	public void resetBuffer() {
//		synchronized ( this.buffers ) {
//			this.buffers.clear();
//			this.cursor =0;
//		}
//	}

	private void offerNewBuffer( Metro metro, JackClient client, JackPosition position ) throws JackException {
		MetroNoteEventBuffer buf = new MetroNoteEventBuffer();
		boolean result = this.logic.processOutputNoteBuffer( metro, this, buf );
		buf.prepare( metro, client, position );

		if ( result ) {
			this.buffers.offer( buf );
		} else {
			metro.unregisterSequence( this );
		}
		// buf.dump();
	}
} 