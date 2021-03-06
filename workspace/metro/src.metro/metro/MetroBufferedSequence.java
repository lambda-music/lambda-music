/*
 * Metro Musical Sequencing Framework written by Atsushi Oka 
 * Copyright 2018 Atsushi Oka
 *
 * This file is part of Metro Musical Sequencing Framework. 
 * 
 * Metro Musical Sequencing Framework is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * Metro Musical Sequencing Framework is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with Metro Musical Sequencing Framework.  If not, see <https://www.gnu.org/licenses/>.
 */

package metro;

import java.lang.invoke.MethodHandles;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;
import java.util.Queue;
import java.util.logging.Level;

import lamu.lib.logging.Logger;

/**
 * 
 * @author Ats Oka
 *
 */
public abstract class MetroBufferedSequence implements MetroSequence, MetroSynchronizable, MetroSynchronizedStarter, MetroSynchronizedStopper  {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }

    private static final boolean DEBUG = false;
    private static final boolean DEBUG_BUF = false;
    public boolean isEnabled() {
        return enabled;
    }
    public void setEnabled(boolean enabled) {
        this.enabled = enabled;
    }

    private volatile boolean enabled = true;

    public abstract <T> void generateBuffer( Metro metro, MetroTrack track, MetroBufferedMidiReceiverFactory<T> bufferFactory );
    
    /**
     * Get the buffered event sets.
     * <p/>
     * This list must be accessed inside a synchronized block with {@link Metro#getMetroLock() }.
     * <pre>
     * synchronized ( this.getMetroTrackLock() ) {
     *    ... getBuffers() ...
     * }
     * </pre>
     */
    private Queue<MetroEventBuffer> buffers = new ArrayDeque<>();
    
    /**
     * This interface defines how 
     * {@link MetroBufferedSequence#calcBufferStatus(Collection, double, long, MetroEventBufferSelector) } 
     * method selects the specified buffer. 
     */
    static interface MetroEventBufferSelector {
    	boolean select( double position, MetroEventBuffer buffer );
    	static MetroEventBufferSelector bySeqNo( long seqNo ) {
			return new MetroEventBufferSelector() {
				@Override
				public boolean select( double position, MetroEventBuffer buffer ) {
					return buffer.getSeqNo() == seqNo;
				}
			};
    	}
    	static MetroEventBufferSelector byPosition( double lookupPosition ) {
			return new MetroEventBufferSelector() {
				@Override
				public boolean select(double position, MetroEventBuffer buffer) {
					return lookupPosition < position + buffer.getLength();
				}
			};
    	}
    }
    
	/**
	 * This method must be called inside a synchronized block with
	 * {@link Metro#getMetroLock() }. See {@link #buffers } for further
	 * information.
	 * 
	 * If the current status is invalid in ways such as being underflow, being not
	 * initialized yet or others, this method returns zero; otherwise, this returns
	 * a newly created {@link MetroTrackBufferStatus} object.
	 * 
	 * @return the current status object; null when the current status is invalid.
	 */
	public static MetroTrackBufferStatus calcBufferStatus( Collection<MetroEventBuffer> buffers, double currentPosition, long currentBufferSeqNo, MetroEventBufferSelector selector ) {
		// the specified buffer
		MetroEventBuffer buffer = null;
		// the current buffer
		MetroEventBuffer currentBuffer = null;
		// the result object
		MetroTrackBufferStatus status = new MetroTrackBufferStatus();
		
		/*        
		 * (Thu, 16 Jul 2020 14:26:48 +0900)
		 *               v current position
		 *            -----------------------------------....
		 *  REF     __|__x____|_____|_______|____|_____|_ ... 
		 * MAIN    |_____x__|_____|____|____|______|_____ ...
		 * 
		 * At first, you have to look up the buffer by seqno to know its position in MAIN.
		 * In the second, you have to look up the buffer by MAIN position in REF
		 * MAIN's position -  REF's position == "THE DIFFERENCE"
		 *  
		 */
		
		// Looking for the starting point (currentBuffer) and the ending point (buffer)
		// and accumulate length of the buffers between the two exclude the ending point.
		double position = 0.0d - currentPosition;

		for ( MetroEventBuffer e : buffers ) {
    		// Look up the starting point:
    		if ( e.getSeqNo() == currentBufferSeqNo ) {
    			if ( currentBuffer == null ) {
    				if ( buffer != null ) {
        				logInfo("calcBufferStatusByBufferSeqNo result=##ILLEGAL01-0##");
        				return null;
    				}
    				currentBuffer = e;
    			} else {
    				logInfo("calcBufferStatusByBufferSeqNo result=##ILLEGAL01-1##");
    				return null;
    			}
    		}
    		
    		// Look up the ending point:
    		if ( selector.select( position, e ) ) {
    			if ( buffer == null ) {
    				if ( currentBuffer == null ) {
        				logInfo("calcBufferStatusByBufferSeqNo result=##ILLEGAL02-0##");
        				return null;
    				}
    				buffer = e;
    			} else {
    				logInfo("calcBufferStatusByBufferSeqNo result=##ILLEGAL02-1##");
    				return null;
    			}
    		}
    		
    		// If the starting point is found
    		if ( currentBuffer != null ) {
    			if ( buffer == null ) {
    				position += e.getLength();
    			} else {
    				break;
    			}
    		}
    	}
    	
    	if ( buffer == null || currentBuffer == null ) {
			logInfo("calcBufferStatusByBufferSeqNo result=##ILLEGAL03##");
			return null;
    	}
    	
    	status.setCurrentBufferSeqNo(currentBuffer.getSeqNo());
    	status.setCurrentBufferLength(currentBuffer.getLength());
    	status.setCurrentBufferPosition(0.0d);
    	
    	status.setBufferSeqNo(buffer.getSeqNo());
    	status.setBufferLength(buffer.getLength());
    	status.setBufferPosition( position );
    	
    	status.setCurrentPosition( currentPosition );
    	
    	return status;
	}    
    
	public MetroTrackBufferStatus getBufferStatus( Metro metro, MetroEventBufferSelector selector ) {
		synchronized ( metro.getMetroLock() ) {
			return calcBufferStatus( this.buffers, this.getPosition(metro), this.currentBufferSeqNo, selector ); 
		}
	}
    

    
    private volatile long totalCurosrMax = Long.MAX_VALUE;
    private volatile long totalCursor = 0;
    private volatile long cursor = 0;
    private volatile long lastLengthInFrames = 0;
    private volatile long lastAccumulatedLength = 0;
    
    public long getCursor() {
        return cursor;
    }
    public void setCursor(long cursor) {
        logInfo("setCursor:" + cursor );
        this.cursor = cursor;
        this.totalCursor = cursor;
    }

    @Override
    public long getCurrentPositionInFrames(Metro metro) {
        return this.getCursor();
    }
    @Override
    public void setCurrentPositionInFrames(Metro metro, long positionInFrames) {
        this.setCursor(positionInFrames);
    }


    
    /**
     * Create a MetroTrack object with default synchronizing status.
     *
     * This constructor is <code>public</code> but this is usually called only from
     * {@link Metro#createTrack(String, Collection, MetroSequence, MetroSyncType, MetroTrack, double)}
     * and users should not call this constructor directory. Though it is still left public
     * for further hacking.
     * @param name
     *            Specifying the identifier of the track.
     * @param tags
     *            Specifying the tag strings. This could be null and treated as an
     *            empty set.
     * @param sequence
     *            Specifying the sequence object to play.
     */
    public MetroBufferedSequence() {
        this.startSynchronizer = MetroTrackSynchronizerBasic.immediate();
    }

    public void removeGracefully(Metro metro, long syncOffset ) {
        logInfo( "removeGracefully:" + syncOffset );
        synchronized ( metro.getMetroLock() ) {
            // added (Sat, 23 May 2020 16:03:20 +0900)
            this.totalCurosrMax = this.totalCursor + (syncOffset * -1);
        }
    }
    @Override
    public void stop( Metro metro, MetroTrack track ) {
        synchronized ( metro.getMetroLock() ) {
            List<MetroTrack> tracksSnapshot = metro.replicateAllTracks();
            long currentLengthInFrames = getCurrentLengthInFrames(metro);
            long syncOffset = stopSynchronizer.syncronizeTrack( metro, track, tracksSnapshot, currentLengthInFrames);
            removeGracefully(metro, syncOffset );
        }
    }
    
    private volatile boolean bufferRequested = true;
    synchronized void requestBuffer() {
        this.bufferRequested = true;
    }
    synchronized void resetBufferRequested() {
        this.bufferRequested = false;
    }
    synchronized boolean isBufferRequested() {
        return bufferRequested;
    }
    
    synchronized void checkBuffer(
			Metro metro, 
			MetroTrack track, 
			long nframes, 
			long measureLengthInFrames, 
			List<MetroMidiEvent> inputMidiEvents, 
			List<MetroMidiEvent> outputMidiEvents,
			List<MetroTrack> tracks, 
			List<MetroTrack> registeringTracks, 
			List<MetroTrack> finalizingTracks, 
			List<MetroTrack> unregisteringTracks ) throws MetroException 
    {
        if ( ! isBufferRequested() ) {
            return;
        } else {
			processBuffer(
					metro, 
					track, 
					nframes, 
					measureLengthInFrames, 
					inputMidiEvents,
					outputMidiEvents,
					tracks,
					registeringTracks, 
					finalizingTracks, 
					unregisteringTracks);
            resetBufferRequested();
        }
    }
    
    
    @Override
    public double getPosition( Metro metro ) {
        synchronized ( metro.getMetroLock() ) {
            if ( lastLengthInFrames < 0 || cursor < 0) {
                return 0;
            } else {
                return (double)( cursor - lastAccumulatedLength + lastLengthInFrames )  / (double)lastLengthInFrames;
            }
        }
    }
    
    @Override
    public long getCurrentLengthInFrames(Metro metro) {
        ArrayList<MetroEventBuffer> buffers;
        long currentBufferSeqNo;
        synchronized ( metro.getMetroLock() ) {
            buffers = new ArrayList<>(this.buffers);
            currentBufferSeqNo = this.currentBufferSeqNo;
        }
        MetroEventBuffer currentBuf =null;
        for ( Iterator<MetroEventBuffer> i = buffers.iterator(); i.hasNext(); ) {
            MetroEventBuffer buf = i.next();
            if ( currentBufferSeqNo == buf.getSeqNo() ) {
                currentBuf=buf;
                break;
            }
        }
        if ( currentBuf == null )
            return 0;
        else
            return currentBuf.getLengthInFrames(); 
    }

    private static final long SEQ_NO_NOT_INITIALIZED = -1;
    
    // ADDED (Sun, 10 May 2020 02:10:20 +0900) BUFFER_SEQ_NO
    /**
     * See {@link MetroBufferedSequence#createdBufferSeqNo}
     */
    private volatile long currentBufferSeqNo = SEQ_NO_NOT_INITIALIZED;

//  @Override
//  public boolean equals(Object obj) {
//      try {
//          return this.name == ((Track)obj).name;
//      } catch ( ClassCastException e ) {
//            Logger.getLogger( Metro.class.getName()).log(Level.WARNING, null, e );
//          return false;
//      }
//  }

// ???
//  public int getCursor() {
//      return cursor;
//  }
//  public void setCursor( int cursor ) {
//      this.cursor = cursor; 
//  }

    
    /*
     * === About `found` flag === ( COMMENT_A )
     * 
     * We have data for some notes in a bar. And we have 
     * to search for the notes between the current area.
     * We are in the area between the last cursor position
     * and the next cursor position which is located n-frames
     * behind it. The n-frame is the parameter nframes.
     * 
     * We have to iterate every note data and see if the note
     * is in the area. This requires that the note data are sorted.
     *    
     * The searching process contains three stages :
     *
     * 1. While between() function returns false,
     * the current position should be before the area.
     * 
     * 2. While between() function returns true,
     * the current position should be in the area.
     *  
     * 3. When between() function return false 
     * after between() function return true,
     * the current position should be after the area.
     * 
     * Note that when it comes to the stage-3, between() 
     * function will not return true again; therefore the 
     * searching process is not necessary to continue anymore.
     * 
     * Asterisk denotes a Note Event Object. 
     *
     * 
     *                    THE SEARCH AREA
     *                    |            |
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

    /* 
     * (Sat, 23 Dec 2017 23:16:25 +0900)
     * 
     * Note : 
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
    
    /*
     * Note : (Sun, 10 May 2020 03:06:29 +0900)
     * 
     *                       ex) currentBufferSeqNo=2
     *                                vvvv
     * |------------|------------|------------|------------|------------|
     * | buf seq0   | buf seq1   | buf seq2   | buf seq3   | buf seq4   |
     * |0.0      1.0|0.0      1.0|0.0      1.0|0.0      1.0|0.0      1.0|
     * |------------|------------|------------|------------|------------|
     *                                                       cursor=4.5    <=== a future buffer           
     *                                          cursor=3.5                 <=== a future buffer          
     *                            [cursor=0.5]                             <=== the current buffer                         
     *                cursor=1.5                                           <=== a past buffer 
     *  cursor=2.5                                                         <=== a past buffer
     * 
     * - We assume the length of all buffer is 1.0d; it may vary in practical. 
     * - Note that some buffer has events which surpass the length of their buffer.
     * - Every buffer has events of which position value is conveyed on a relative coordinate system.  
     * - The `cursor` value must translate to the current buffer's coordinate sytem.
     * 
     * |============|
     * |   buf1     |                        
     * |0.0      1.0     *(1.5)  << This event also to be processed. 
     * |============|          
     *              |============|
     *              |   buf2     |                        
     *              |0.0      1.0|
     *              |============|          
     *                            |============|
     *                            |   buf3     |                        
     *     This, too ===> *(-0.5)  0.0      1.0|
     *                            |============|          
     *
     * The current implementation failed to process in this way.
     *                   
     */
    

    @Override
    public void process( 
        Metro metro, 
        MetroTrack track,
        long nframes, 
        long measureLengthInFrames, 
        List<MetroMidiEvent> inputMidiEvents, 
        List<MetroMidiEvent> outputMidiEvents, 
        List<MetroTrack> tracks, 
        List<MetroTrack> registeringTracks, 
        List<MetroTrack> finalizingTracks,
        List<MetroTrack> unregisteringTracks ) throws MetroException 
    {

        // Start the process.
        long currentCursor;
        long nextCursor;
        long currentBufferSeqNo;
        Collection<MetroEventBuffer> buffers;
        
        synchronized ( metro.getMetroLock() ) {
            synchronized ( metro.getMetroLock() ) {
                // Update the buffer list. 
                checkBuffer(
                		metro, 
                		track, 
                		nframes, 
                		measureLengthInFrames, 
                		inputMidiEvents, 
                		outputMidiEvents, 
                		tracks, 
                		registeringTracks, 
                		finalizingTracks, 
                		unregisteringTracks );

                // Before everything, initialize synchronizing status with the track.
                // In most case, this method does nothing and returns immediately.
                synchronizeTrack( metro, track, tracks, measureLengthInFrames );
                
                // 
                reprepareTrack( metro, measureLengthInFrames );
                


                currentCursor      = this.cursor;
                nextCursor         = currentCursor + nframes;
                currentBufferSeqNo = this.currentBufferSeqNo;
                buffers = new ArrayList<>( this.buffers );
            }

            {
                if ( currentBufferSeqNo == SEQ_NO_NOT_INITIALIZED ) {
                    return;
                }

                // This keeps negative offset value for the current cursor position. 
                long cursorOffset = 0;

                // Total the bar length of all buffers which are before the current buffer.
                for ( Iterator<MetroEventBuffer> i = buffers.iterator(); i.hasNext(); ) {
                    MetroEventBuffer buf = i.next();
                    if ( currentBufferSeqNo == buf.getSeqNo() ) {
                        break;
                    }
                    cursorOffset -= buf.getLengthInFrames();
                }

                for ( Iterator<MetroEventBuffer> i = buffers.iterator(); i.hasNext(); ) {
                    MetroEventBuffer buf = i.next();

                    // Translate the cursorOffset to the buffer-local coordinate system. 
                    long from  = currentCursor - cursorOffset;
                    long to    = nextCursor    - cursorOffset;

                    // Search all of the corresponding events and process  them. 
                    searchEventBuffer( metro, 
                        buf.getMetroEvents(),
                        outputMidiEvents, 
                        tracks, 
                        registeringTracks,
                        finalizingTracks, 
                        unregisteringTracks, from, to );

                    // move the cursor offset.
                    cursorOffset = cursorOffset + buf.getLengthInFrames();
                }


                {
                    boolean done = false;
                    for(;;){
                        MetroEventBuffer currBuf=null;
                        for ( Iterator<MetroEventBuffer> i = buffers.iterator();i.hasNext(); ) {
                            MetroEventBuffer buf = i.next();
                            if ( currentBufferSeqNo == buf.getSeqNo() ) {
                                currBuf = buf;
                                break;
                            }
                        }
                        if ( currBuf == null ) {
                            // `currBuf == null` implies either buffer-underflow or buffer-overflow.
                            // A special measurement is required; but since it is not likely happening, 
                            // ignore it for now.
                            logWarn( String.format( 
                                "ERROR === CANNOT FIND CURRENT BUFFER === :%5s nextCursor=%3s bufferSeqNo=%s bufcount=%s", 
                                track.getName(), nextCursor, currentBufferSeqNo, buffers.size()));
                            done = true;
                        } else {
                            long lengthInFrames = currBuf.getLengthInFrames();

                            if ( lengthInFrames <= nextCursor ) {
                                nextCursor -= lengthInFrames;
                                currentBufferSeqNo ++;
                                done = true;
                                if (DEBUG) {
                                    logInfo( String.format( "update-cursor:%5s next=%3s/%3s bufferSeqNo=%s bufcount=%s", 
                                        track.getName(), nextCursor, lengthInFrames, currentBufferSeqNo, buffers.size() ));
                                }
                                continue;
                            }
                        }
                        break;
                    }
                    if ( done ) {
                        metro.notifyTrackChange("update");
                        requestBuffer();
                    }
                }
                //            logInfo( "this.cursor:" + getName() + ":"  + this.cursor );
            }
            synchronized ( metro.getMetroLock() ) {
                this.cursor = nextCursor  ;
                this.totalCursor += nframes;
                this.currentBufferSeqNo = currentBufferSeqNo;

                // (Sat, 23 May 2020 16:04:56 +0900)
                if ( this.totalCurosrMax <= this.totalCursor ) {
                    logInfo( "The track is being removed. " + this.totalCurosrMax );

                    unregisteringTracks.add( track );
                }
            }
        }
        //            if ( DEBUG && false )
        //                logInfo( "progressCursor(2):" + currentCursor + "/" + (bufs.isEmpty() ? "empty" : bufs.peek().getLengthInFrames()  ));
    }

    
    protected boolean searchEventBuffer( Metro metro, 
        List<MetroEvent> inputMidiEventList, 
        List<MetroMidiEvent> outputMidiEventList, 
        List<MetroTrack> tracks,
        List<MetroTrack> registeringTrackList, 
        List<MetroTrack> finalizingTracks,
        List<MetroTrack> unregisteringTrackList, long from, long to) 
    {
        if ( this.isEnabled() ) { // <<< ADDED (Sun, 03 May 2020 17:59:12 +0900)
            boolean found= false;
            for ( Iterator<MetroEvent> ie = inputMidiEventList.iterator(); ie.hasNext();  ) {
                MetroEvent e = ie.next();

                if ( e.isBetweenInFrames( from, to ) ) {
                    found = true;
                    e.process( metro, from );
                    if ( e instanceof MetroEventOutput )
                        ((MetroEventOutput)e).processOutput( outputMidiEventList, tracks, registeringTrackList, finalizingTracks, unregisteringTrackList );
                } else {
                    if ( found ) // SEE COMMENT_A (Fri, 02 Aug 2019 19:20:40 +0900)
                        break;
                }
            }
            return found;
        } else {
            return false;
        }
    }

    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    
    private volatile boolean syncPrepared = false;
    public void resetSyncStatus() {
        syncPrepared = false;
        enabled = true;
        
        buffers.clear();
        cursor = 0;
        lastLengthInFrames = 0;
        lastAccumulatedLength = 0;
    }
    
    private MetroTrackSynchronizer startSynchronizer = MetroTrackSynchronizerBasic.immediate();
    @Override
    public MetroTrackSynchronizer getStartSynchronizer() {
        return startSynchronizer;
    }
    @Override
    public void setStartSynchronizer(MetroTrackSynchronizer startSynchronizer) {
        this.startSynchronizer = startSynchronizer == null ?  MetroTrackSynchronizerBasic.immediate() : startSynchronizer;
    }
    private MetroTrackSynchronizer stopSynchronizer = MetroTrackSynchronizerBasic.immediate();
    @Override
    public MetroTrackSynchronizer getStopSynchronizer() {
        return stopSynchronizer ;
    }
    @Override
    public void setStopSynchronizer(MetroTrackSynchronizer stopSynchronizer) {
        this.stopSynchronizer = stopSynchronizer == null ? MetroTrackSynchronizerBasic.immediate() : stopSynchronizer;
    }


    /**
     * This method will be called only once by the Metro messaging thread when
     * MetroTrack is added to registered Track.
     * @param metro TODO
     * @param track TODO
     * @param tracks TODO
     * @param measureLengthInFrames
     * @throws MetroException 
     */
    public void synchronizeTrack( Metro metro, MetroTrack track, List<MetroTrack> tracks, long measureLengthInFrames ) throws MetroException {
        if ( ! this.syncPrepared ) {
            this.syncPrepared = true;
            long positionInFrames = 
                this.startSynchronizer.syncronizeTrack( metro, track, tracks, measureLengthInFrames );
            this.setCurrentPositionInFrames(metro, positionInFrames);
        }
    }

    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    private volatile long lastMeasureLengthInFrames = -1;
    /**
     * This method watches the value specified in measureLengthInFrames and if it detects any change on the value,
     * it invokes {@link #reprepareTrack(Metro, long)} method to re-prepare the track object.
     * 
     * @param metro
     * @param measureLengthInFrames
     * @throws MetroException
     * @see
     */
    public void reprepareTrack(Metro metro, long measureLengthInFrames) throws MetroException {
        if ( this.lastMeasureLengthInFrames != measureLengthInFrames ) {
            this.lastMeasureLengthInFrames = measureLengthInFrames;
            reprepareTrack0(metro, measureLengthInFrames );
        }
    }

    // Created (Thu, 07 May 2020 03:14:15 +0900)
    /**
     * The position of a note is stored as a float value; the value has to be converted to an integer value
     * in order to be processed by JACKAudio. In this system, this conversion is called "preparing track".
     * 
     * @param metro
     * @param measureLengthInFrames
     * @throws MetroException
     */
    protected void reprepareTrack0( Metro metro, long measureLengthInFrames ) throws MetroException {
        
        // Note (Wed, 06 Nov 2019 05:47:26 +0900)
        // This method could be called by setting tempo procedures and 
        // that time could be at the time when it is not opened.
        // In that case, we should ignore the request and should not 
        // raise execptions.
        // 
        // Note.2 (Wed, 06 May 2020 02:37:17 +0900)
        // This comment is moved from Metro#reprepareTrack() method.
        // The reprepareTrack() method was ditched from the code.
        // This comment should be rewritten.

        long prevLengthInFrame;
        long lengthInFrame;
        Queue<MetroEventBuffer> buffers2;
        long cursor;
        long totalCursor;
        
        synchronized ( metro.getMetroLock() ) {
            prevLengthInFrame = -1;
            lengthInFrame = -1;
            buffers2 = new ArrayDeque<>( this.buffers );
            cursor = this.cursor;
            totalCursor = this.totalCursor;
        }
        
        {
            {
                MetroEventBuffer headBuffer = buffers2.peek();
                if ( headBuffer != null )
                    prevLengthInFrame = headBuffer.getLengthInFrames();
            }

            // double ratio = magnifyCursorPosition( prevBeatsPerMinute, beatsPerMinute );
            for ( MetroEventBuffer buffer : buffers2 ) {
                buffer.prepare(measureLengthInFrames, false);
            }

            {
                MetroEventBuffer headBuffer = buffers2.peek();
                if ( headBuffer != null )
                    lengthInFrame = headBuffer.getLengthInFrames();
            }

            double ratio = (double)lengthInFrame / (double)prevLengthInFrame;

            if ( 0< ratio && 1.0d!= ratio ) {
                // System.out.println( "ratio: " + ratio );
                // System.out.println( "prev cursor: " + cursor );

                cursor      = (long) Math.round( ((double)cursor)            * ratio );

                // NOTE : (Wed, 30 Oct 2019 05:35:28 +0900)
                // multiplying by ratio on `this.totalCursor` may not work as expected 
                // but since changing tempo while recording is an improper action so
                // we ignore the risk for now.
                totalCursor = (long) Math.round( ((double)totalCursor)       * ratio );


                // System.out.println( "after cursor: " + cursor );
                // System.out.println( "lengthInFrame    : " + lengthInFrame );
                // System.out.println( "prevLengthInFrame: " + prevLengthInFrame );
                // this.lastLengthInFrame = (long) Math.round( ((double)this.lastLengthInFrame) * ratio );
            }
        }
        synchronized ( metro.getMetroLock() ) {
            this.cursor = cursor;
            this.totalCursor = totalCursor;
        }

    }

    private static final double thresholdBackwardBufferLength = 2.0d; 
    private static final double thresholdForewardBufferLength = 2.0d; 
    private static final long thresholdBackwardBufferCount = 2; 
    private static final long thresholdForewardBufferCount = 1; 


    private final ArrayList<MetroEventBuffer> share_buffers = new ArrayList<>();
    private final ArrayList<MetroEventBuffer> share_buffersToAdd  = new ArrayList<>();
    private final ArrayList<MetroEventBuffer> share_buffersTmp  = new ArrayList<>();

    
    // This method was formerly checkBuffer()
    // Moved from MetroSequence (Fri, 17 Jul 2020 16:04:24 +0900)
    /**
     * 
     * @param metro
     * @param track
     * @param measureLengthInFrames
     * @throws MetroException
     */
    public void processBuffer(
			Metro metro, 
			MetroTrack track, 
			long nframes, 
			long measureLengthInFrames, 
			List<MetroMidiEvent> inputMidiEvents, 
			List<MetroMidiEvent> outputMidiEvents,
			List<MetroTrack> tracks, 
			List<MetroTrack> registeringTracks, 
			List<MetroTrack> finalizingTracks, 
			List<MetroTrack> unregisteringTracks ) throws MetroException 
    {
        double backwardBufferLength;
        double forewardBufferLength;
        int backwardBufferCount;
        int forewardBufferCount;
        long currentBufferSeqNo;
        share_buffers.clear();
        share_buffersToAdd.clear();
        share_buffersTmp.clear(); 
        ArrayList<MetroEventBuffer> buffers      = share_buffers;
        ArrayList<MetroEventBuffer> buffersToAdd = share_buffersToAdd;
        ArrayList<MetroEventBuffer> buffersTmp   = share_buffersTmp;; 
        long buffersToRemove;

        
        // Creating a snapshot of the current status.
        synchronized ( metro.getMetroLock() ) { // << ADDED synchronided (Sun, 30 Sep 2018 11:45:13 +0900)
            buffers.addAll( this.buffers );
            currentBufferSeqNo = this.currentBufferSeqNo;
            buffersToRemove = 0;
        }
        
        {
            {
                {
                    for(;;) {
                        // 1. Examine the current buffer list's status.
                        backwardBufferLength = 0;
                        forewardBufferLength = 0;
                        backwardBufferCount = 0;
                        forewardBufferCount = 0;

                        // Look up the current buffer.
                        boolean found =false;
                        long theLastBufferSeqNo = -1;
                        for ( Iterator<MetroEventBuffer> i=buffers.iterator(); i.hasNext(); ) {
                            MetroEventBuffer buf = i.next();
                            long seqNo = buf.getSeqNo();
                            theLastBufferSeqNo = seqNo;
                            if (! found ) {
                                if ( currentBufferSeqNo == seqNo ) {
                                    found = true;
                                    continue;
                                }
                                backwardBufferLength+= buf.getLength();
                                backwardBufferCount++;
                            } else {
                                forewardBufferLength += buf.getLength();
                                forewardBufferCount++;
                            }
                        }
                        
                        if ( found )
                            break;
                        
                        // If the current buffer is not found, that implicates that there are insufficient buffers 
                        // on the buffer list.

                        // 2. In case the buffer underflows:
                        {
                            /*
                             *  (Tue, 12 May 2020 03:27:03 +0900)
                             *  
                             * Note that (theLastBufferSeqNo==-1) means that there is no buffer on the buffer list.
                             * 
                             * Also note that when (theLastBufferSeqNo==-1) and the currentBufferSeqNo==0 which is 
                             * very likely on the start up time. In that case, it should create one buffer.
                             * This is the reason why the initial value of `theLastBufferSeqNo` is -1 which causes
                             * it to execute only once.
                             */
                            if ((currentBufferSeqNo == SEQ_NO_NOT_INITIALIZED) || (theLastBufferSeqNo < currentBufferSeqNo) ) {
                                // Initialize the seq no; this value will be reflected to `this.currentBufferSeqNo`.
                                currentBufferSeqNo = 0;
                                {
                                	// MODIFIED (Sat, 18 Jul 2020 00:57:29 +0900) >>>
                                    // buffersTmp.clear();
                                    // for ( long i=theLastBufferSeqNo; i<currentBufferSeqNo; i++ ) {
                                    // 	   this.createNewBuffer( metro, track, measureLengthInFrames, buffersTmp );
                                    // }
                                	buffersTmp.clear();
                                	// Limit the maximum count of the loop in order to avoid infinite loop.
                                    for( int i=0;i<4096; i++) {
                                    	if ( theLastBufferSeqNo + buffersTmp.size() < currentBufferSeqNo ) {
                                    		this.createNewBuffer( metro, track, measureLengthInFrames, buffersTmp );
                                    	} else {
                                    		break;
                                    	}
                                    }
                                	// MODIFIED (Sat, 18 Jul 2020 00:57:29 +0900) <<<

                                    buffers.addAll( buffersTmp );
                                    buffersToAdd.addAll( buffersTmp );
                                }
                            } else {
                                /*
                                 *  In this case, it might be highly unusual. It is very likely not happening;
                                 *  therefore ignore it for now. (Tue, 12 May 2020 03:27:03 +0900)
                                 */
                                logError( "****** UNUSUAL CASE ********", new Error("CHECK THE SITUATION") );
                            }
                        }
                        // 3. Restart this procedure. In the next time, it should found the current buffer. 
                        continue;
                    }
                }

                if ( DEBUG ) {
                    logInfo( "==============" );
                    logInfo( String.format( "name=%s", track.getName() ));
                    logInfo( String.format( "backwardBufferLength=%s", backwardBufferLength ));
                    logInfo( String.format( "backwardBufferCount=%s", backwardBufferCount   ));
                    logInfo( String.format( "forewardBufferLength=%s", forewardBufferLength ));
                    logInfo( String.format( "forewardBufferCount=%s", forewardBufferCount   ));
                    logInfo( "---" );
                }


                // Create bufferes ahead.
                {
                    
                    for ( int i=0; i<256; i++ ) {
                        if ( (forewardBufferCount<thresholdForewardBufferCount) || ( forewardBufferLength < thresholdForewardBufferLength ) ) {
                            buffersTmp.clear();
                            double length = this.createNewBuffer( metro, track, measureLengthInFrames, buffersTmp );
                            buffers.addAll( buffersTmp );
                            buffersToAdd.addAll( buffersTmp );
                            // TODO
                            forewardBufferLength += length;
                            forewardBufferCount += buffersTmp.size();
                        } else {
                            break;
                        }
                    }
                }

                // Remove bufferes behind.
                {
                    for ( Iterator<MetroEventBuffer> i=buffers.iterator(); i.hasNext(); ) {
                        MetroEventBuffer buf = i.next();
                        double length = buf.getLength();
                        if ( (thresholdBackwardBufferCount < backwardBufferCount) && ( thresholdBackwardBufferLength < backwardBufferLength - length) ) {
                            if ( DEBUG ) {
                                logInfo( String.format( "removing backward buffer: length=%s count=%s" , backwardBufferLength, backwardBufferCount ) );
                            }
                            backwardBufferLength -= length;
                            backwardBufferCount--;
                            i.remove();
                            buffersToRemove++;
                            if ( DEBUG ) {
                                logInfo( String.format( "removed  backward buffer: length=%s count=%s" , backwardBufferLength, backwardBufferCount ) );
                            }
                        } else {
                            break;
                        }
                    }
                }

                if ( DEBUG ) {
                    logInfo( String.format( "name=%s", track.getName() ));
                    logInfo( String.format( "backwardBufferLength=%s", backwardBufferLength ));
                    logInfo( String.format( "backwardBufferCount=%s", backwardBufferCount   ));
                    logInfo( String.format( "forewardBufferLength=%s", forewardBufferLength ));
                    logInfo( String.format( "forewardBufferCount=%s", forewardBufferCount   ));
                    logInfo( "" );
                }
            }
        }
        
        // Reflect the modification to the current status.
        synchronized ( metro.getMetroLock() ) { // << ADDED synchronided (Sun, 30 Sep 2018 11:45:13 +0900)
            this.currentBufferSeqNo= currentBufferSeqNo;
            this.buffers.addAll(buffersToAdd);
            for ( int i=0; i<buffersToRemove; i++ )
                this.buffers.remove();
        }
    }

    
    
    final class MetroEventBufferFactory implements MetroBufferedMidiReceiverFactory<MetroEvent> {
    	MetroEventBufferFactory() {
		}
    	private long barLengthInFrames;
    	List<MetroEventBuffer> bufferList;
    	double length;
    	long actualLength;
    	long endPosition;
    	void preInit( long barLengthInFrames, List<MetroEventBuffer> bufferList ) {
    		this.bufferList = bufferList;
			this.barLengthInFrames = barLengthInFrames;
    		this.length = 0;
    		this.endPosition =-1;
    	}
		@Override
    	public MetroBufferedMidiReceiver<MetroEvent> create() {
            MetroEventBuffer buf = MetroEventBuffer.create();
            this.bufferList.add( buf );
    		return buf;
    	}
		void postInit() {
			if ( bufferList.isEmpty() ) {
				MetroEventBuffer b = MetroEventBuffer.create();
				b.end();
				bufferList.add(b);
			}
			for ( MetroEventBuffer buffer : bufferList ) {
				initNewBuffer(buffer, barLengthInFrames);
				this.length += buffer.getLength();
				this.actualLength += buffer.getActualLength();
				if ( buffer.endCalled() && this.endPosition < 0 ) {
					this.endPosition = this.actualLength;
				}
				if ( DEBUG_BUF && ( buffer.size() >0 ) )
					logInfo( buffer.dump("") );
			}
		}
		public double getLength() {
			return this.length;
		}
		public long getActualLength() {
			return actualLength;
		}
		public boolean endCalled() {
			return 0<=this.endPosition;
		}
		public long getEndPosition() {
			return this.endPosition;
		}
		@Override
		public String toString() {
			StringBuilder sb = new StringBuilder();
			for ( Object o : bufferList )  {
				sb.append( Objects.toString(o) ).append( "\n\n" );
			}
			return sb.toString();
		}
    }
    final MetroEventBufferFactory bufferFactory = new MetroEventBufferFactory();
    
    
    // ADDED (Sun, 10 May 2020 02:10:20 +0900) BUFFER_SEQ_NO
    /**
     * See {@link MetroBufferedSequence#currentBufferSeqNo
     */
    private volatile long createdBufferSeqNo = 0;
    private void initNewBuffer( MetroEventBuffer buf, long barLengthInFrames ) {
        buf.prepare( barLengthInFrames, true );
        buf.setSeqNo( createdBufferSeqNo ++ );
    }

    private double createNewBuffer( Metro metro, MetroTrack track, long barLengthInFrames, List<MetroEventBuffer> result ) {
        double length=0.0d;
        synchronized ( metro.getMetroLock() ) {
            {
                boolean endCalled=false;
                double endingLength=0.0d;
                // MODIFIED (Sat, 18 Jul 2020 01:30:22 +0900) >>>
                //  {
                //    	MetroEventBuffer buf = MetroEventBuffer.create();
                //    	this.generateBuffer( metro, track, buf );
                //    	this.initNewBuffer(buf, barLengthInFrames);
                //
                //    	if ( DEBUG_BUF && ( buf.size() >0 ) )
                //    		logInfo( buf.dump("") );
                //
                //    	result.add( buf );
                //    	length += buf.getLength();
                //
                //    	// 1. ending 1
                //    	if ( buf.endCalled() ) {
                //    		endCalled = true;
                //    		endingLength = buf.getActualLength();
                //    		if ( endingLength < 1 )
                //    			endingLength = 1;
                //    	}
                //  }
                {
                	this.bufferFactory.preInit(barLengthInFrames,result);
                    this.generateBuffer( metro, track, bufferFactory );
                    this.bufferFactory.postInit();
                    length += this.bufferFactory.getLength();
                    
                    // 1. ending 1
                    if ( this.bufferFactory.endCalled() ) {
                        endCalled = true;
                        endingLength = this.bufferFactory.getEndPosition();
                        if ( endingLength < 1 )
                            endingLength = 1;
                    }
                }
                // MODIFIED (Sat, 18 Jul 2020 01:30:22 +0900) <<<


                if ( endCalled ) {
                    if ( DEBUG )
                        logInfo( "offerNewBuffer(" + track.getName() + ") ENDING started");

                    // 2. ending 2
                    {
                        if ( DEBUG )
                            logInfo( "offerNewBuffer(): endingDone is true" );

                        MetroEventBuffer buf = MetroEventBuffer.create();
                        buf.length( 1.0 );
                        initNewBuffer(buf, barLengthInFrames);
                        result.add( buf );
                        length += buf.getLength();
                        
                    }

                    // 3. ending 3
                    {
                        MetroEventBuffer buf = MetroEventBuffer.create();
//                        buf.exec( endingLength , new Runnable() {
//                            @Override
//                            public void run() {
//                                if ( DEBUG )
//                                    logInfo( "offerNewBuffer(" + track.getName() + ") UNREGISTER THIS" );
//                                synchronized ( metro.getMetroLock() ) {
//                                    try {
//                                        metro.unregisterTrack( Arrays.asList( track ) );
//                                    } finally {
//                                        metro.notifyTrackChange("update");
//                                    }
//                                }
//                            }
//                        });
                        buf.exet(0, 
                            MetroTrackManipulatorBasic.unregister( 
                                MetroTrackSelectorBasic.constant( track )));
                        buf.length( endingLength  );
                        initNewBuffer(buf, barLengthInFrames);
                        result.add( buf );
                        length += buf.getLength();
                    }
                    
                }
            }
        }
        return length;
    }
} 
