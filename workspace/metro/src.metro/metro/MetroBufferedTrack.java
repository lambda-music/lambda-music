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
import java.util.Queue;
import java.util.logging.Level;

import lamu.lib.log.Logger;

/*
 * To acquire further understanding about buffering of this Metro framework,
 * activate log output to see what come out from it. See the keyword UNDERSTANDING_METRO_BUFFERING .
 * ( I mean, select the CAPITALIZED_KEYWORD and hit CTRL-F thing to find the string in this file. )
 */

/**
 * 
 * @author Ats Oka
 *
 */
public abstract class MetroBufferedTrack extends MetroSyncTrack  {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }

    private static final boolean DEBUG = false;

    /*
     * Memo : I think this is not necessary anymore; this used to be margin length
     * to fetch the next buffer or something that I don't quite remember. With a
     * quick investigation to set it to one and it seems that it does not cause any
     * problem. So
     * 
     * The value one means that it is not used anymore. The related code might be
     * unused,too. But it needs more investigation to delete it; so I left it be
     * like this for now. (Sat, 03 Aug 2019 12:20:59 +0900)
     */
    private static final double MARGIN_LENGTH = 1;   

    
//    /**
//     * 
//     */
//    private final Metro metro;
    
    public boolean isEnabled() {
        return enabled;
    }
    public void setEnabled(boolean enabled) {
        this.enabled = enabled;
    }

    private transient boolean enabled = true;

//  private static final int BUFFER_SIZE = 2;
// MODIFIED >>> (Mon, 08 Jul 2019 20:29:49 +0900)
//  private static final double MARGIN_LENGTH = 2;   
//  private static final double MARGIN_LENGTH = 9;   
// MODIFIED <<< 

    public abstract <T> void processBuffered( Metro metro, MetroBufferedMidiReceiver<T> buffer );

    
    /*
     * This list object must be referred with synchronization.
     * synchronized ( this.getMetroTrackLock() ) {
     *    ... getBuffers() ...
     * }
     */
    private Queue<MetroEventBuffer> buffers = new ArrayDeque<>();
    
    private transient long totalCursor = 0;
    private transient long cursor = 0;
    private transient long lastLengthInFrames = 0;
    private transient long lastAccumulatedLength = 0;
    
    private transient boolean ending = false;
    private transient boolean endingDone = false;
    private transient double endingLength = 0;
    
    public long getCursor() {
        return cursor;
    }
    public void setCursor(long cursor) {
        this.cursor = cursor;
        this.totalCursor = cursor;
    }

    @Override
    public long getCurrentPositionInFrames(Metro metro) {
        return this.getCursor();
    }
    @Override
    public void setCurrentPositionInFrames(Metro metro, long position) {
        this.setCursor(position);
    }

    
    @Override
    public void resetSyncStatus() {
        super.resetSyncStatus();
        enabled = true;
        
        buffers.clear();
        cursor = 0;
        lastLengthInFrames = 0;
        lastAccumulatedLength = 0;
        
        ending = false;
        endingDone = false;
        endingLength = 0;
        
//        eventListenerable.clearEventListeners();
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
    public MetroBufferedTrack( Object name, Collection<Object> tags, 
        MetroSyncType syncType, MetroSyncTrack syncTrack, double syncOffset) {
        super(name,tags,syncType,syncTrack,syncOffset);
    }
    
    
//    public Object getMetroTrackLock() {
//        // TODO COUNTERMEASURE_FOR_LOCKING (Mon, 23 Sep 2019 08:33:32 +0900)
//        return this.metro.getMetroLock();
////        return buffers;
//    }

//    @Override
//    public Object getMetroLock() {
//        // TODO COUNTERMEASURE_FOR_LOCKING (Mon, 23 Sep 2019 08:33:32 +0900)
//        return this.metro.getMetroLock();
//    }
    
    // Moved from MetroSyncTrack (Tue, 05 May 2020 16:47:27 +0900)
    // Moved from MetroTrack (Thu, 07 May 2020 13:03:00 +0900)
    /**
     * Removes this track from the parent Metro object. Calling this method has no
     * effect if this track does not belong to the specified Metro object, This
     * method calls {@link Metro#unregisterTrack(Collection)} method to remove this
     * track. This behavior is a default implementation. Any subclasses which
     * inherit this class should properly reimplement their desirable behavior.
     * 
     * @param metro
     *    The parent Metro object.
     */

    public void removeGracefully(Metro metro) {
        if ( this.ending ) {
            logWarn( "removeGracefully was called; but the track is already removed." );
        } else {
            logInfo( "removeGracefully was called; the ending flag is set." );
            this.ending = true;
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
        synchronized ( metro.getMetroLock() ) {
            Queue<MetroEventBuffer> bufs = this.buffers;
            if ( bufs.size() == 0 )
                return 0;
            else
                return bufs.peek().getLengthInFrames(); 
        }
    }

    // ADDED (Sun, 10 May 2020 02:10:20 +0900) BUFFER_SEQ_NO
    /**
     * See {@link MetroBufferedTrack#createdBufferSeqNo}
     */
    private volatile long currentBufferSeqNo = 0;

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
     * - Assume the length of all buffer is 1.0d; it may vary in practical. 
     * - Note that some buffer have events which surpass the length of their buffer.
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
    public void progressCursor( Metro metro, long nframes, List<MetroMidiEvent> inputMidiEventList, List<MetroMidiEvent> outputMidiEventList ) throws MetroException {
        synchronized ( metro.getMetroLock() ) {
            long currentCursor = this.cursor;
            long nextCursor    = currentCursor + nframes;
            long bufferSeqNo   = currentBufferSeqNo;
            Collection<MetroEventBuffer> bufs = new ArrayList<>( this.buffers );
            
            // This keeps negative offset value for the current cursor position. 
            long cursorOffset = 0;

            // Total the bar length of all buffers which are before the current buffer.
            for ( Iterator<MetroEventBuffer> ibuf = bufs.iterator(); ibuf.hasNext(); ) {
                MetroEventBuffer buf = ibuf.next();
                if ( bufferSeqNo == buf.getSeqNo() ) {
                    break;
                }
                cursorOffset -= buf.getBarLengthInFrames();
            }

            for ( Iterator<MetroEventBuffer> ibuf = bufs.iterator(); ibuf.hasNext(); ) {
                MetroEventBuffer buf = ibuf.next();

                // Translate the cursorOffset to the buffer-local coordinate system. 
                long from  = currentCursor - cursorOffset;
                long to    = nextCursor    - cursorOffset;

                // Search all of the corresponding events and process  them. 
                searchEventBuffer( metro, buf, outputMidiEventList, from, to );

                // move the cursor offset.
                cursorOffset = cursorOffset + buf.getLengthInFrames();
            }


            {

                boolean done = false;
                for(;;){
                    MetroEventBuffer currBuf=null;
                    for ( Iterator<MetroEventBuffer> ibuf = bufs.iterator();ibuf.hasNext(); ) {
                        MetroEventBuffer buf = ibuf.next();
                        if ( bufferSeqNo == buf.getSeqNo() ) {
                            currBuf = buf;
                            break;
                        }
                    }
                    if ( currBuf == null ) {
                        // `currBuf == null` implies either buffer-underflow or buffer-overflow.
                        // A special measurement is required; but since it is not likely happening, 
                        // ignore it for now.
                        if ( DEBUG ) {
                            logWarn("###########################################################");
                            logWarn("####################ERRRORRRRRRRRRRRRR#####################");
                            logWarn("###########################################################");
                        }
                        done = true;
                    } else {
                        long lengthInFrames = currBuf.getLengthInFrames();
                        
                        if ( lengthInFrames <= nextCursor ) {
                            nextCursor -= lengthInFrames;
                            bufferSeqNo ++;
                            done = true;
                            if (DEBUG) {
                                logInfo( String.format( "update-cursor:%5s next=%3s/%3s bufferSeqNo=%s bufcount=%s", getName(), nextCursor, lengthInFrames, bufferSeqNo, bufs.size() ));
                            }
                            continue;
                        }
                    }
                    break;
                }
                if ( done ) {
                    metro.notifyTrackChange("update");
                }
            }
            
            this.cursor = nextCursor  ;
            this.totalCursor += nframes;
            this.currentBufferSeqNo = bufferSeqNo;
            
//            if ( DEBUG && false )
//                logInfo( "progressCursor(2):" + currentCursor + "/" + (bufs.isEmpty() ? "empty" : bufs.peek().getLengthInFrames()  ));
        }
    }

    public void progressCursorOld( Metro metro, long nframes, List<MetroMidiEvent> inputMidiEventList, List<MetroMidiEvent> outputMidiEventList ) throws MetroException {
        synchronized ( metro.getMetroLock() ) {
            long currentCursor = this.cursor;
            long nextCursor    = currentCursor + nframes;
            
            // This keeps negative offset value for the current cursor position. 
            long cursorOffset = 0;

            // Total the bar length of all buffers which are before the current buffer.
            for ( Iterator<MetroEventBuffer> ibuf = this.buffers.iterator(); ibuf.hasNext(); ) {
                MetroEventBuffer buf = ibuf.next();
                if ( currentBufferSeqNo <=buf.getSeqNo() )
                    break;
                
                cursorOffset -= buf.getBarLengthInFrames();
            }
            

            for ( Iterator<MetroEventBuffer> ibuf = this.buffers.iterator(); ibuf.hasNext(); ) {
                MetroEventBuffer buf = ibuf.next();
                
                // Translate the cursorOffset to the buffer-local coordinate system. 
                long from  = currentCursor - cursorOffset;
                long to    = nextCursor    - cursorOffset;
                
                // Search all of the corresponding events and process them. 
                searchEventBuffer( metro, buf, outputMidiEventList, from, to );
                
                // move the cursor offset.
                cursorOffset = cursorOffset + buf.getLengthInFrames();
            }
            
            if ( false ) {
                long accumulatedLength = 0;
                int pollCount = 0;
                long lengthInFrame = -1;
                for (Iterator<MetroEventBuffer> it = this.buffers.iterator();it.hasNext(); ) {
                    MetroEventBuffer b=it.next();
                    accumulatedLength += b.getLengthInFrames();
                    
                    if (  accumulatedLength < ( currentCursor - (long)(b.getBarLengthInFrames() * MARGIN_LENGTH ) ) ) {
                        pollCount ++;
                    }
                    if ( currentCursor < accumulatedLength ) {
                        lengthInFrame = b.getLengthInFrames();
                        break;
                    }
                }
                
                int polledCount = 0;
                for (Iterator<MetroEventBuffer> it = this.buffers.iterator();it.hasNext(); ) {
                    MetroEventBuffer b = it.next();
                    if ( polledCount < pollCount  ) {
                        polledCount ++;
                        long currentLengthInFrames = b.getLengthInFrames();
                        currentCursor -= currentLengthInFrames;
                        nextCursor -= currentLengthInFrames;
                        it.remove();
//                      this.buffers.poll();
                        if (DEBUG) {
                            logInfo( String.format( 
                                "progressCursor: currentLengthInFrames:%d currentCursor:%d lengthInFrames:%d", 
                                    currentLengthInFrames , currentCursor, b.getLengthInFrames() ) );
                        }

                    } else {
                        break;
                    }
                }
                
                if ( 0< pollCount )
                    metro.notifyTrackChange("update");

                this.cursor = nextCursor;
                this.totalCursor += nframes;
                this.lastLengthInFrames = lengthInFrame;
                this.lastAccumulatedLength = accumulatedLength;
            }
            
            this.cursor = nextCursor;
            this.totalCursor += nframes;
            
            if ( DEBUG && false )
                logInfo( "progressCursor(2):" + currentCursor + "/" + (this.buffers.isEmpty() ? "empty" : this.buffers.peek().getLengthInFrames()  ));
        }
    }
    
    private boolean searchEventBuffer(Metro metro, MetroEventBuffer buf, List<MetroMidiEvent> output, 
        long from,
        long to) 
    {
        if ( this.isEnabled() ) { // <<< ADDED (Sun, 03 May 2020 17:59:12 +0900)
            boolean found= false;
            for ( Iterator<MetroEvent> ie = buf.getMetroEventList().iterator(); ie.hasNext();  ) {
                MetroEvent e = ie.next();

                if ( e.isBetweenInFrames( from, to ) ) {
                    found = true;
                    e.process( metro, from );
                    if ( e instanceof MetroMidiEvent ) {
                        output.add( (MetroMidiEvent)e );
                    }
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

    /**
     * This method will be called only once by the Metro messaging thread when
     * MetroTrack is added to registered Track.
     * @param metro TODO
     * @param barLengthInFrames
     * @throws MetroException 
     */
    @Override
    public void prepareSyncStatus( Metro metro, long barLengthInFrames ) throws MetroException {
        MetroSyncTrack.prepareSyncStatus( metro, this, barLengthInFrames );
    }

    @Override
    public void reprepareSyncStatus( Metro metro, long barLengthInFrames ) throws MetroException {
        
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

        synchronized ( metro.getMetroLock() ) {
            long prevLengthInFrame = -1;
            long lengthInFrame = -1;
            {
                MetroEventBuffer headBuffer = this.buffers.peek();
                if ( headBuffer != null )
                    prevLengthInFrame = headBuffer.getLengthInFrames();
            }

            // double ratio = magnifyCursorPosition( prevBeatsPerMinute, beatsPerMinute );
            for ( MetroEventBuffer buffer : this.buffers ) {
                buffer.prepare(barLengthInFrames, false);
            }

            {
                MetroEventBuffer headBuffer = this.buffers.peek();
                if ( headBuffer != null )
                    lengthInFrame = headBuffer.getLengthInFrames();
            }

            double ratio = (double)lengthInFrame / (double)prevLengthInFrame;

            if ( 0< ratio && 1.0d!= ratio ) {
                // System.out.println( "ratio: " + ratio );
                // System.out.println( "prev cursor: " + cursor );

                this.cursor      = (long) Math.round( ((double)this.cursor)            * ratio );

                // NOTE : (Wed, 30 Oct 2019 05:35:28 +0900)
                // multiplying by ratio on `this.totalCursor` may not work as expected 
                // but since changing tempo while recording is an improper action so
                // we ignore the risk for now.
                this.totalCursor = (long) Math.round( ((double)this.totalCursor)       * ratio );


                // System.out.println( "after cursor: " + cursor );
                // System.out.println( "lengthInFrame    : " + lengthInFrame );
                // System.out.println( "prevLengthInFrame: " + prevLengthInFrame );
                // this.lastLengthInFrame = (long) Math.round( ((double)this.lastLengthInFrame) * ratio );
            }
        }
    }

    private long getTotalBufferLengthInFrames() {
        long total = 0;
        for ( MetroEventBuffer b : this.buffers ) {
            total += b.getLengthInFrames();
        }
        total -= cursor;
        
        // UNDERSTANDING_METRO_BUFFERING
        if ( DEBUG )
            logInfo( "getTotalBufferLength(" + getName() + "):(count:" + this.buffers.size() + ") " + total );
        return total;
    }
    
    private static final double thresholdBackwardBufferLength = 2.0d; 
    private static final double thresholdForewardBufferLength = 2.0d; 

    @Override
    public void processBuffer( Metro metro, long barLengthInFrames) throws MetroException {
        synchronized ( metro.getMetroLock() ) { // << ADDED synchronided (Sun, 30 Sep 2018 11:45:13 +0900)
            super.processBuffer(metro, barLengthInFrames);
            
            double backwardBufferLength = 0;
            double forewardBufferLength = 0;
            int backwardBufferCount = 0;
            int forewardBufferCount = 0;
            long bufferSeqNo = currentBufferSeqNo;
            boolean found =false;
            for ( Iterator<MetroEventBuffer> i=this.buffers.iterator(); i.hasNext(); ) {
                MetroEventBuffer buf = i.next();
                if (! found ) {
                    if ( bufferSeqNo == buf.getSeqNo() ) {
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

            if ( DEBUG ) {
                logInfo( "==============" );
                logInfo( String.format( "name=%s", getName() ));
                logInfo( String.format( "backwardBufferLength=%s", backwardBufferLength ));
                logInfo( String.format( "backwardBufferCount=%s", backwardBufferCount   ));
                logInfo( String.format( "forewardBufferLength=%s", forewardBufferLength ));
                logInfo( String.format( "forewardBufferCount=%s", forewardBufferCount   ));
                logInfo( "---" );
            }

            // Remove bufferes behind.
            {
                for ( Iterator<MetroEventBuffer> i=this.buffers.iterator(); i.hasNext(); ) {
                    MetroEventBuffer buf = i.next();
                    double length = buf.getLength();
                    if ( (1 < backwardBufferCount) && ( thresholdBackwardBufferLength < backwardBufferLength - length) ) {
                        if ( DEBUG ) {
                            logInfo( String.format( "removing backward buffer: length=%s count=%s" , backwardBufferLength, backwardBufferCount ) );
                        }
                        backwardBufferLength -= length;
                        backwardBufferCount--;
                        i.remove();
                        if ( DEBUG ) {
                            logInfo( String.format( "removed  backward buffer: length=%s count=%s" , backwardBufferLength, backwardBufferCount ) );
                        }
                    } else {
                        break;
                    }
                }
            }

            // Create bufferes ahead.
            for ( int i=0; i<256; i++ ) {
                if ( (forewardBufferCount<1) || ( forewardBufferLength < thresholdForewardBufferLength ) ) {
                    MetroEventBuffer buf = this.offerNewBuffer( metro, barLengthInFrames );
                    forewardBufferLength += buf.getLength();
                    forewardBufferCount++;
                } else {
                    break;
                }
            }
            if ( DEBUG ) {
                logInfo( String.format( "name=%s", getName() ));
                logInfo( String.format( "backwardBufferLength=%s", backwardBufferLength ));
                logInfo( String.format( "backwardBufferCount=%s", backwardBufferCount   ));
                logInfo( String.format( "forewardBufferLength=%s", forewardBufferLength ));
                logInfo( String.format( "forewardBufferCount=%s", forewardBufferCount   ));
                logInfo( "" );
            }

        }
    }
    
    /*
     * Lazy initializing field
     */
    private long cacheUpdateThreshold = -1;
    private static final int MIN_UPDATE_THRESHOLD = 256;
    private static final int MAX_UPDATE_THRESHOLD = 44100*4;
    
    public void processBufferOld( Metro metro, long barLengthInFrames) throws MetroException {
        synchronized ( metro.getMetroLock() ) { // << ADDED synchronided (Sun, 30 Sep 2018 11:45:13 +0900)
            super.processBuffer(metro, barLengthInFrames);
            
//          if ( this.buffers.size() < BUFFER_SIZE ) {
//              this.offerNewBuffer( metro, client, position );
//          }
            
//          MODIFIED >>> (Fri, 02 Aug 2019 16:52:08 +0900)
//          while ( getAccumulatedLength() < MARGIN_LENGTH * MARGIN_LENGTH ) {
//              this.offerNewBuffer( metro, client, position );
//          }
            
            long updateThreshold = cacheUpdateThreshold;
            if ( updateThreshold < 0 ) {
                double metroUpdateThreshold = metro.getUpdateSequenceThreshold();
                
                updateThreshold = (long)((double)barLengthInFrames * metroUpdateThreshold);
                String message = "" + updateThreshold +"=" + barLengthInFrames + "*" + metroUpdateThreshold +" (updateThreshold = barInFrames * metroUpdateThreshold)";
                
                if ( updateThreshold  < MIN_UPDATE_THRESHOLD ) {
                    if ( DEBUG )
                        logWarn( "Too small update-threshold value causes system to become unstable. " + message );
                    updateThreshold = 256;
                    if ( DEBUG )
                        logWarn( "We ignored the specified update-threshold value; reset to " + updateThreshold  + "." );
                } else if ( MAX_UPDATE_THRESHOLD < updateThreshold ) {
                    if ( DEBUG )
                        logWarn( "Too large update-threshold value causes system to become unstable. " + message );
                    updateThreshold = MAX_UPDATE_THRESHOLD;
                } else {
                    cacheUpdateThreshold = updateThreshold;
                    if ( DEBUG )
                        logWarn( "We set updateThreshold value. " + message );
                }
            }

            // UNDERSTANDING_METRO_BUFFERING 
            if ( DEBUG )
                logInfo( "checkBuffer(" + getName() + "):thre:" + updateThreshold  );
            
            while ( getTotalBufferLengthInFrames() < updateThreshold ) {
                this.offerNewBuffer( metro, barLengthInFrames );
            }
//          MODIFIED <<< (Fri, 02 Aug 2019 16:52:08 +0900)
        }
    }
//    protected void clearBuffer() {
//        synchronized ( this.getMetroTrackLock() ) {
//            this.buffers.clear();
//            this.cursor =0;
//            this.totalCursor = 0;
//        }
//    }
    
//  public void resetBuffer() {
//      synchronized ( this.getMetroTrackLock() ) {
//          this.buffers.clear();
//          this.cursor =0;
//      }
//  }


    // ADDED (Sun, 10 May 2020 02:10:20 +0900) BUFFER_SEQ_NO
    /**
     * See {@link MetroBufferedTrack#currentBufferSeqNo
     */
    private volatile long createdBufferSeqNo = 0;
    private void offerNewBufferProc( MetroEventBuffer buf, long barLengthInFrames ) {
        buf.prepare( barLengthInFrames, true );
        buf.setSeqNo( createdBufferSeqNo ++ );
        this.buffers.offer( buf );
    }

    private MetroEventBuffer offerNewBuffer( Metro metro, long barLengthInFrames ) {
        synchronized ( metro.getMetroLock() ) {
            
//          logInfo( "offerNewBuffer:" );
            if ( this.ending ) {
//              logInfo( "offerNewBuffer:ending (" + this.name  + ")");

                if ( this.endingDone ) {
                    if ( DEBUG )
                        logInfo( "offerNewBuffer(): endingDone is true" );
                    MetroEventBuffer buf = MetroEventBuffer.create();
                    buf.length( 1.0 );
                    offerNewBufferProc(buf, barLengthInFrames);
//                    this.buffers.offer( receiver );
                    return buf;
                } else {
                    if ( DEBUG )
                        logInfo( "offerNewBuffer(" + getName() + ") setting true endingDone " );
                    this.endingDone = true;

                    MetroEventBuffer buf = MetroEventBuffer.create();
                    buf.exec( this.endingLength , new Runnable() {
                        @Override
                        public void run() {
                            if ( DEBUG )
                                logInfo( "offerNewBuffer(" + getName() + ") UNREGISTER THIS" );
                            synchronized ( metro.getMetroLock() ) {
                                try {
                                    metro.unregisterTrack( MetroBufferedTrack.this );
                                } finally {
                                    metro.notifyTrackChange("update");
                                }
                            }
                        }
                    });
                    buf.length( this.endingLength  );
                    offerNewBufferProc(buf, barLengthInFrames);
                    return buf;
//                    this.buffers.offer( buf );
                }
                
            } else {
                // ??? is this really necessary? (Sun, 19 Apr 2020 09:48:14 +0900)
                // REMOVED (Sun, 19 Apr 2020 16:05:49 +0900) 
                // initialize the current thread
                // this.metro.getThreadInitializerCollection().initialize();

//              logInfo( "offerNewBuffer:normal (" + this.name  + ")");
                MetroEventBuffer buf = MetroEventBuffer.create();
                this.processBuffered( metro, buf );
                offerNewBufferProc(buf, barLengthInFrames);

                if ( DEBUG && ( buf.size() >0 ) )
                    logInfo( buf.dump("") );
                
//                this.buffers.offer( buf );
                
                if ( buf.endCalled() ) {
                    if ( DEBUG )
                        logInfo( "offerNewBuffer(" + getName() + ") ENDING started");
                    this.ending = true;
                    this.endingLength = buf.getActualLength();
                    if ( this.endingLength < 1 )
                        this.endingLength = 1;
                    // buf.dump();
                } else {
                    if ( DEBUG )
                        logInfo( "offerNewBuffer(" + getName() + ") CONTINUE" );
                }
                return buf;
            }
        }
    }
} 
