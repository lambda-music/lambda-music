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
    private static final Logger LOGGER = Logger.getLogger(MetroNoteEventBufferSequence.class.getName());

//	int flag=0;

	static void logInfo( String msg ) {
    	System.err.println( msg );
		// Logger.getLogger(MetroNoteEventBufferSequence.class.getName()).log(Level.INFO, msg );
    }
    static void logError( String msg, Throwable e ) {
		LOGGER.log(Level.SEVERE, msg, e);
    }

	public enum SyncType {
		IMMEDIATE, PARALLEL, SERIAL,  
	}
	/**
	 * 
	 */
	private final Metro metro;
	protected final String name;
	protected Set<String> tags;
	protected boolean enabled = true;

	@SuppressWarnings("unused")
	private static final boolean DEBUG = false;

	// static final int BUFFER_SIZE = 2;
	private static final int BUFFER_SIZE = 2;

	int id = (int) (Math.random()* Integer.MAX_VALUE);

	private transient SyncType syncType;
	private transient MetroNoteEventBufferSequence syncSequence;
	private transient double syncOffset=0.0d;
	private BlockingQueue<MetroNoteEventBuffer> buffers = new LinkedBlockingQueue<>();
	protected transient int cursor = 0;
	protected transient int lastLengthInFrame = 0;
	protected final MetroLogic logic;
	transient boolean ending = false;
	
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
		if ( lastLengthInFrame < 0 || cursor < 0) {
			return 0;
		} else {
			return (double)cursor / (double)lastLengthInFrame;
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


	protected void progressCursor( int nframes, List<MetroMidiEvent> result ) throws JackException {
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
									e0.getOffsetInFrames() - actualCursor, 
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

//			if ( flag ==0 ) {
//				logInfo( "cursor(after): " + this.cursor );
//			}
			
			this.cursor = nextCursor;
			this.lastLengthInFrame = lengthInFrames; 
			if ( Metro.DEBUG )
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
					prevLengthInFrame = headBuffer.lengthInFrames;
			}
				
			// double ratio = magnifyCursorPosition( prevBeatsPerMinute, beatsPerMinute );
			for ( MetroNoteEventBuffer buffer : this.buffers ) {
				buffer.prepare(metro, client, position, false);
			}
			
			{
				MetroNoteEventBuffer headBuffer = this.buffers.peek();
				if ( headBuffer != null )
					lengthInFrame = headBuffer.lengthInFrames;
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

	protected  void checkBuffer( Metro metro, JackClient client, JackPosition position ) throws JackException {
		synchronized ( this.buffers ) { // << ADDED synchronided (Sun, 30 Sep 2018 11:45:13 +0900)
			if ( this.buffers.size() < BUFFER_SIZE ) {
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
				buf.exec( 0, new Runnable() {
					@Override
					public void run() {
						metro.unregisterSequence( MetroNoteEventBufferSequence.this );
					}
				});
				buf.setLength(1);
				buf.prepare( metro, client, position, true );
				this.buffers.offer( buf );
				
			} else {
				MetroNoteEventBuffer buf = new MetroNoteEventBuffer();
				boolean result = this.logic.processOutputNoteBuffer( metro, this, buf );
				buf.prepare( metro, client, position, true );

				// buf.dump();
				
				this.buffers.offer( buf );
				
				if ( result ) {
				} else {
					this.ending = true;
				}
				// buf.dump();
			}
		}
	}
} 