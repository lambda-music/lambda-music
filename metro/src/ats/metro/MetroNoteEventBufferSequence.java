package ats.metro;

import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.jaudiolibs.jnajack.JackClient;
import org.jaudiolibs.jnajack.JackException;
import org.jaudiolibs.jnajack.JackPosition;

public class MetroNoteEventBufferSequence implements MetroPlayer, MetroLock {
	static final Logger LOGGER = Logger.getLogger(MetroNoteEventBufferSequence.class.getName());
	static void logError(String msg, Throwable e) {
		LOGGER.log(Level.SEVERE, msg, e);
	}
	static void logInfo(String msg) {
		// LOGGER.log(Level.INFO, msg);
		System.err.println(msg);
	}
	static void logWarn(String msg) {
		LOGGER.log(Level.WARNING, msg);
	}


	public enum SyncType {
		IMMEDIATE, PARALLEL, SERIAL,  
	}
	
	
	/**
	 * 
	 */
	private final Metro metro;
	/**
	 * Note that the String object which is stored in name field must be interned.  
	 */
	protected final String name;
	protected Set<String> tags;
	protected boolean enabled = true;

	private static final boolean DEBUG = false;

//	private static final int BUFFER_SIZE = 2;
	private static final double MARGIN_LENGTH = 2;   

	int id = (int) (Math.random()* Integer.MAX_VALUE);

	private transient SyncType syncType;
	private transient MetroNoteEventBufferSequence syncSequence;
	private transient double syncOffset=0.0d;
	private BlockingQueue<MetroNoteEventBuffer> buffers = new LinkedBlockingQueue<>();
	protected transient int cursor = 0;
	protected transient int lastLengthInFrames = 0;
	protected transient int lastAccumulatedLength = 0;
	
	protected final MetroLogic logic;
	transient boolean ending = false;
	transient double endingLength = 0;
	
	public MetroNoteEventBufferSequence( Metro metro, String name, Collection<String> tags, MetroLogic logic, SyncType syncType, MetroNoteEventBufferSequence syncSequence, double syncOffset ) {
//		LOGGER.info( "BufferSequence(" + name + ") : " + tags + " : " + syncType + " : " + syncOffset );
		this.name = name.intern();
		if ( tags == null )
			this.tags = new HashSet<>();
		else
			this.tags = (new HashSet<>( tags ));
		this.metro = metro;
		this.logic = logic;
		this.syncType = syncType;
		this.syncSequence = syncSequence;
		this.syncOffset = syncOffset;
		
		logic.setPlayer( this );
	}
	
	@Override
	public Object getMetroLock() {
		return this.metro.lock;
	}
	
	public MetroLogic getLogic() {
		return this.logic;
	}
	@Override
	public String getPlayerName() {
		return name;
	}
	@Override
	public Set<String> getPlayerTags() {
		return tags;
	}
	
	@Override
	public boolean isPlayerEnabled() {
		return enabled;
	}
	@Override
	public void setPlayerEnabled(boolean enabled) {
		this.enabled = enabled;
	}
	@Override
	public void playerRemove( boolean graceful ) {
		if ( graceful ) {
			this.ending = true;
		} else {
			metro.unregisterSequence( this );
		}
	}
	@Override
	public double getPosition() {
		if ( lastLengthInFrames < 0 || cursor < 0) {
			return 0;
		} else {
			return (double)( cursor - lastAccumulatedLength + lastLengthInFrames )  / (double)lastLengthInFrames;
		}
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
	
	/*
	 * === About `found` flag ===
	 * There are three stages :
	 *
	 * 1. While between() function returns false,
	 * the current position should be before the area.
	 * 2. While between() function returns true,
	 * the current position should be in the area. 
	 * 3. When between() function return false 
	 * after between() function return true,
	 * the current position should be after the area.
	 * 
	 * If it entered to the stage-3, between() function will not
	 * return true again; therefore the searching process is
	 * not necessary to continue anymore.
	 * 
	 * * Asterisk denotes a Note Event Object. 
	 * 
	 * 
	 *                    | THE AREA   |
	 *  *                 |            |
	 *    *               |            |
	 *         *          |            |
	 *              *     |            |
	 *                    | *          |
	 *                    |     *      |
	 *                    |            |
	 *                    |          * |
	 *                    |            |*  
	 *                    |            |        *
	 *                    |            |           *
	 *                    |            |             *
	 */


	static int BUFFER_REMOVAL_STRATEGY = 3;
	protected void progressCursor2( int nframes, List<MetroMidiEvent> result ) throws JackException {
		synchronized ( this.buffers ) {
			this.metro.clearAllPorts();

			int currentCursor = this.cursor;
			int nextCursor = currentCursor + nframes;
			
			// This keeps negative offset value for the current cursor position. 
			int cursorOffset = 0;

			//    	System.out.println( cursor + "/" + nextCursor );
//			int accrossCount = 0;
			
			for ( Iterator<MetroNoteEventBuffer> ibuf = this.buffers.iterator(); ibuf.hasNext(); ) {
				MetroNoteEventBuffer buf = ibuf.next();

				int actualCursor     = currentCursor - cursorOffset;
				int actualNextCursor = nextCursor    - cursorOffset;
				
//				System.out.println( "actualCursor : " + actualCursor );
//				System.out.println( "actualNextCursor : " + actualNextCursor );
//				if ( flag ==0 ) {
//					flag = 1;
//					logInfo( "cursor : " + this.cursor );
//					logInfo( "actualCursor : " + actualCursor );
//					logInfo( "actualNextCursor : " + actualNextCursor );
//				}

				//    		System.out.println( "AFTER::::" );
				//    		buf.dump();
				boolean found= false;
				for ( Iterator<MetroAbstractEvent> ie = buf.iterator(); ie.hasNext();  ) {
					MetroAbstractEvent e = ie.next();
					
					if ( e.between( actualCursor, actualNextCursor ) ) {
						//		    			System.out.println("VALUE" + ( e.getOffsetInFrames() - this.cursor ) );
						//        			System.out.println( e.("***" ));
						found = true;
//						System.out.println( e.dump("") ); 
						
						if ( e instanceof MetroAbstractMidiEvent ) {
							MetroAbstractMidiEvent e0 = (MetroAbstractMidiEvent) e;
							
							result.add( new MetroMidiEvent( 
									e0.getOutputPortNo(), 
									e0.offsetInFrames - actualCursor, 
									e0.getData() 
									) );
						} else if ( e instanceof MetroAbstractSchemeProcedureEvent ) {
							((MetroAbstractSchemeProcedureEvent)e).execute( metro );
						} else {
							LOGGER.log( Level.SEVERE, "Unknown Class " + e.getClass().getName() );
						}
						

						//        			System.out.println( "event.getData()" + event.getData() );
						//        			System.out.println( "event.getData()" + Integer.toUnsignedString(event.getData()[0] , 2 ) );
					} else {
						if ( found )
							break;
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
			
			switch ( BUFFER_REMOVAL_STRATEGY ) {
				case 1 : {
					int lengthInFrames=-1;
					for (;;) {
						if ( this.buffers.isEmpty() )
							break;
				
						lengthInFrames = this.buffers.peek().getLengthInFrames();

						// XXX 
						if (  lengthInFrames <  currentCursor ) {
							currentCursor -= lengthInFrames;
							nextCursor -= lengthInFrames;
							this.buffers.poll();
							metro.notifyCheckBuffer();
						} else {
							break;
						}
					}
					this.cursor = nextCursor;
					this.lastLengthInFrames = lengthInFrames;
					break;
				}
				case 2 : {
					int lengthInFrames=-1;
					for (;;) {

						// version 1
						// if ( this.buffers.isEmpty() )
						//	break;
						// lengthInFrames = this.buffers.peek().getLengthInFrames();

						// version 2 >>>
						if ( this.buffers.size() < 2 )
							break;

						{
							// get the second element
							Iterator<MetroNoteEventBuffer> it = this.buffers.iterator();
							it.next();
							lengthInFrames = it.next().getLengthInFrames();
						}
						// <<<

						// XXX 
						if (  lengthInFrames <  currentCursor ) {
							currentCursor -= lengthInFrames;
							nextCursor -= lengthInFrames;
							this.buffers.poll();
							metro.notifyCheckBuffer();
						} else {
							break;
						}
					}
					this.cursor = nextCursor;
					this.lastLengthInFrames = lengthInFrames;
					break;
				}
				case 3 : {
					int lengthInFrames=-1;
					for (;;) {
						if ( this.buffers.size() < 2 )
							break;

						{
							// get the second element
							Iterator<MetroNoteEventBuffer> it = this.buffers.iterator();
							it.next();
							lengthInFrames = it.next().getLengthInFrames();
						}
						// <<<

						// XXX 
						if (  lengthInFrames <  currentCursor ) {
							currentCursor -= lengthInFrames;
							nextCursor -= lengthInFrames;
							this.buffers.poll();
							metro.notifyCheckBuffer();
						} else {
							break;
						}
					}				
					this.cursor = nextCursor;
					this.lastLengthInFrames = lengthInFrames;
					break;
				}
				default : {
					throw new Error("internal error");
				}
			}

//			if ( flag ==0 ) {
//				logInfo( "cursor(after): " + this.cursor );
//			}
			
			if ( Metro.DEBUG && false)
				logInfo( currentCursor + "/" + (this.buffers.isEmpty() ? "empty" : this.buffers.peek().getLengthInFrames()  ));
		}
	}

	/* 
	 * (Sat, 23 Dec 2017 23:16:25 +0900)
	 * 1. 
	 * This "if" statement should not be like :
	 *     buf.getLengthInFrames() <= nextCursor
	 * This should be :
	 *     buf.getLengthInFrames() <   nextCursor
	 *  
	 * Because there might be a note on the last position.
	 * This may often be a note off event.
	 * 
	 * 2.
	 *    this.cursor= this.cursor - buf.getLengthInFrames();
	 *    
	 * In this statement `this.cursor` could be lower than zero.
	 * This value is used when when we call JackMidi.eventWrite() afterwards 
	 * as offset value of the current frame.  
	 * 
	 */
	protected void progressCursor( int nframes, List<MetroMidiEvent> result ) throws JackException {
		synchronized ( this.buffers ) {
			this.metro.clearAllPorts();

			int currentCursor = this.cursor;
			int nextCursor = currentCursor + nframes;
			
			// This keeps negative offset value for the current cursor position. 
			int cursorOffset = 0;

			for ( Iterator<MetroNoteEventBuffer> ibuf = this.buffers.iterator(); ibuf.hasNext(); ) {
				MetroNoteEventBuffer buf = ibuf.next();

				int actualCursor     = currentCursor - cursorOffset;
				int actualNextCursor = nextCursor    - cursorOffset;
				
				boolean found= false;
				for ( Iterator<MetroAbstractEvent> ie = buf.iterator(); ie.hasNext();  ) {
					MetroAbstractEvent e = ie.next();
					
					if ( e.between( actualCursor, actualNextCursor ) ) {
						found = true;
						e.process( metro, actualCursor, actualNextCursor, nframes, result);
					} else {
						if ( found )
							break;
					}
				}

				cursorOffset = cursorOffset + buf.getLengthInFrames();
			}
			
			{
				int accumulatedLength = 0;
				int pollCount = 0;
				int lengthInFrame = -1;
				for (Iterator<MetroNoteEventBuffer> it = this.buffers.iterator();it.hasNext(); ) {
					MetroNoteEventBuffer b=it.next();
					accumulatedLength += b.getLengthInFrames();
					
					if (  accumulatedLength < ( currentCursor - (int)(b.getBarInFrames() * MARGIN_LENGTH ) ) ) {
						pollCount ++;
					}
					if ( currentCursor < accumulatedLength ) {
						lengthInFrame = b.getLengthInFrames();
						break;
					}
				}
				
				int polledCount = 0;
				for (Iterator<MetroNoteEventBuffer> it = this.buffers.iterator();it.hasNext(); ) {
					MetroNoteEventBuffer b = it.next();
					if ( polledCount < pollCount  ) {
						polledCount ++;
						int currentLengthInFrames = b.getLengthInFrames();
						currentCursor -= currentLengthInFrames;
						nextCursor -= currentLengthInFrames;
						it.remove();
//						this.buffers.poll();
						if (DEBUG)
							logInfo( String.format( "currentLengthInFrames:%d currentCursor:%d ", currentLengthInFrames , currentCursor ) );

					} else {
						break;
					}
				}
				
				if ( 0< pollCount )
					metro.notifyCheckBuffer();

				this.cursor = nextCursor;
				this.lastLengthInFrames = lengthInFrame;
				this.lastAccumulatedLength = accumulatedLength;
			}
			
			if ( Metro.DEBUG && false)
				logInfo( currentCursor + "/" + (this.buffers.isEmpty() ? "empty" : this.buffers.peek().getLengthInFrames()  ));
		}
	}

	protected void prepare( int barInFrames ) throws JackException {
		int offset = (int) (-1.0d * this.syncOffset * barInFrames);

		switch ( this.syncType ) {
			case IMMEDIATE :
			{
				this.cursor = offset;
				logInfo( "prepare(immediate):" + this.cursor );
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

	protected  void reprepare( Metro metro, JackClient client, JackPosition position, 
			double prevBeatsPerMinute, double beatsPerMinute ) throws JackException 
	{
		synchronized ( this.buffers ) {
			int prevLengthInFrame = -1;
			int lengthInFrame = -1;
			{
				MetroNoteEventBuffer headBuffer = this.buffers.peek();
				if ( headBuffer != null )
					prevLengthInFrame = headBuffer.getLengthInFrames();
			}
				
			// double ratio = magnifyCursorPosition( prevBeatsPerMinute, beatsPerMinute );
			for ( MetroNoteEventBuffer buffer : this.buffers ) {
				buffer.prepare(metro, client, position, false);
			}
			
			{
				MetroNoteEventBuffer headBuffer = this.buffers.peek();
				if ( headBuffer != null )
					lengthInFrame = headBuffer.getLengthInFrames();
			}
			
			double ratio = (double)lengthInFrame / (double)prevLengthInFrame; 
			if ( 0< ratio && 1.0d!= ratio ) {
				// System.out.println( "ratio: " + ratio );
				// System.out.println( "prev cursor: " + cursor );
				this.cursor = (int) Math.round( ((double)this.cursor)            * ratio );
				// System.out.println( "after cursor: " + cursor );
				// System.out.println( "lengthInFrame    : " + lengthInFrame );
				// System.out.println( "prevLengthInFrame: " + prevLengthInFrame );
				// this.lastLengthInFrame = (int) Math.round( ((double)this.lastLengthInFrame) * ratio );
			}
		}

	}

	private double getAccumulatedLength() {
		double accumulatedLength = 0;
		for ( MetroNoteEventBuffer b : this.buffers ) {
			accumulatedLength += b.getLength();
		}
//		logInfo( "accumulatedLength" + accumulatedLength );

		return accumulatedLength;
	}
	protected  void checkBuffer( Metro metro, JackClient client, JackPosition position ) throws JackException {
		synchronized ( this.buffers ) { // << ADDED synchronided (Sun, 30 Sep 2018 11:45:13 +0900)
//			if ( this.buffers.size() < BUFFER_SIZE ) {
//				this.offerNewBuffer( metro, client, position );
//			}
			while ( getAccumulatedLength() < MARGIN_LENGTH * MARGIN_LENGTH ) {
				this.offerNewBuffer( metro, client, position );
			}
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
		synchronized ( this.buffers ) {
			if ( this.ending ) {
				MetroNoteEventBuffer buf = new MetroNoteEventBuffer();
				buf.exec( this.endingLength , new Runnable() {
					@Override
					public void run() {
						System.err.println( "UNREGISTER THIS" );
						metro.unregisterSequence( MetroNoteEventBufferSequence.this );
					}
				});
				buf.setLength(this.endingLength);
				buf.prepare( metro, client, position, true );
				this.buffers.offer( buf );
				
			} else {
				MetroNoteEventBuffer buf = new MetroNoteEventBuffer();
				boolean result = this.logic.processBuffered( metro, this, buf );
				buf.prepare( metro, client, position, true );

				if ( DEBUG && ( buf.size() >0 ) )
					buf.dump();
				
				this.buffers.offer( buf );
				
				if ( result ) {
				} else {
					this.ending = true;
					this.endingLength = buf.getActualLength();
					if ( this.endingLength < 1 )
						this.endingLength = 1;
				}
				// buf.dump();
			}
		}
	}
	@Override
	public boolean equals(Object obj) {
		if ( obj instanceof MetroNoteEventBufferSequence ) {
			return ((MetroNoteEventBufferSequence)obj).name == this.name;
		} else {
			return false;
		}
	}
	@Override
	public int hashCode() {
		return this.name.hashCode() * 2;
	}
} 