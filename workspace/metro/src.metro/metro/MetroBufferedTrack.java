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
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.logging.Level;

import org.jaudiolibs.jnajack.JackException;

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

    private BlockingQueue<MetroEventBuffer> buffers = new LinkedBlockingQueue<>();
    private transient int totalCursor = 0;
    private transient int cursor = 0;
    private transient int lastLengthInFrames = 0;
    private transient int lastAccumulatedLength = 0;
    
    private transient boolean ending = false;
    private transient boolean endingDone = false;
    private transient double endingLength = 0;
    
    public int getCursor() {
        return cursor;
    }
    public void setCursor(int cursor) {
        this.cursor = cursor;
        this.totalCursor = cursor;
    }

    @Override
    public int getCurrentPositionInFrames(Metro metro) {
        return this.getCursor();
    }
    @Override
    public void setCurrentPositionInFrames(Metro metro, int position) {
        this.setCursor(position);
    }

    
    
    /*
     * This must be used with synchronization.
     * synchronized ( this.getMetroTrackLock() ) {
     *    ... getBuffers() ...
     * }
     */
    public BlockingQueue<MetroEventBuffer> getBuffers() {
        return buffers;
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
    public MetroBufferedTrack( Object name, Collection<Object> tags ) {
        super(name,tags);
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
    
    @Override
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
    public int getCurrentLengthInFrames(Metro metro) {
        synchronized ( metro.getMetroLock() ) {
            BlockingQueue<MetroEventBuffer> bufs = this.getBuffers();
            if ( bufs.size() == 0 )
                return 0;
            else
                return bufs.peek().getLengthInFrames(); 
        }
    }
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
    @Override
    public void progressCursor( Metro metro, int nframes, List<MetroMidiEvent> inputMidiEventList, List<MetroMidiEvent> outputMidiEventList ) throws MetroException {
        // REMOVED (Wed, 06 May 2020 08:12:20 +0900) >>>
        // // Moved from Metro class (Mon, 04 May 2020 22:02:08 +0900)
        // this.getSequence().processDirect( metro, nframes, totalCursor, inputMidiEventList, outputMidiEventList );

        synchronized ( metro.getMetroLock() ) {
            
            // This method is actually called twice; may be unnecessary. (Fri, 01 May 2020 16:56:38 +0900)
            try {
                metro.clearAllPorts();
            } catch (JackException e1) {
                // This is very likely removed in a near future.
                throw new MetroException(e1);
            }

            int currentCursor = this.cursor;
            int nextCursor = currentCursor + nframes;
            
            // This keeps negative offset value for the current cursor position. 
            int cursorOffset = 0;

            for ( Iterator<MetroEventBuffer> ibuf = this.buffers.iterator(); ibuf.hasNext(); ) {
                MetroEventBuffer buf = ibuf.next();

                int actualCursor     = currentCursor - cursorOffset;
                int actualNextCursor = nextCursor    - cursorOffset;
                if ( this.isEnabled() ) { // <<< ADDED (Sun, 03 May 2020 17:59:12 +0900)
                    boolean found= false;
                    for ( Iterator<MetroEvent> ie = buf.getMetroEventList().iterator(); ie.hasNext();  ) {
                        MetroEvent e = ie.next();

                        if ( e.isBetweenInFrames( actualCursor, actualNextCursor ) ) {
                            found = true;
                            e.process( metro, actualCursor );
                            if ( e instanceof MetroMidiEvent ) {
                                outputMidiEventList.add( (MetroMidiEvent)e );
                            }
                        } else {
                            if ( found ) // SEE COMMENT_A (Fri, 02 Aug 2019 19:20:40 +0900)
                                break;
                        }
                    }
                }

                cursorOffset = cursorOffset + buf.getLengthInFrames();
            }
            
            {
                int accumulatedLength = 0;
                int pollCount = 0;
                int lengthInFrame = -1;
                for (Iterator<MetroEventBuffer> it = this.buffers.iterator();it.hasNext(); ) {
                    MetroEventBuffer b=it.next();
                    accumulatedLength += b.getLengthInFrames();
                    
                    if (  accumulatedLength < ( currentCursor - (int)(b.getBarLengthInFrames() * MARGIN_LENGTH ) ) ) {
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
                        int currentLengthInFrames = b.getLengthInFrames();
                        currentCursor -= currentLengthInFrames;
                        nextCursor -= currentLengthInFrames;
                        it.remove();
//                      this.buffers.poll();
                        if (DEBUG)
                            logInfo( String.format( "progressCursor: currentLengthInFrames:%d currentCursor:%d lengthInFrames:%d", 
                                currentLengthInFrames , currentCursor, b.getLengthInFrames() ) );

                    } else {
                        break;
                    }
                }
                
                if ( 0< pollCount )
                    metro.notifyTrackChange();

                this.cursor = nextCursor;
                this.totalCursor += nframes;
                this.lastLengthInFrames = lengthInFrame;
                this.lastAccumulatedLength = accumulatedLength;
            }
            
            if ( DEBUG && false )
                logInfo( "progressCursor(2):" + currentCursor + "/" + (this.buffers.isEmpty() ? "empty" : this.buffers.peek().getLengthInFrames()  ));
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
    public void prepareSyncStatus( Metro metro, int barLengthInFrames ) throws MetroException {
        MetroSyncTrack.prepareSyncStatus( metro, this, barLengthInFrames );
    }

    @Override
    public void reprepareSyncStatus( Metro metro, int barLengthInFrames ) throws MetroException {
        
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

        try {
            synchronized ( metro.getMetroLock() ) {
                int prevLengthInFrame = -1;
                int lengthInFrame = -1;
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
                    
                    this.cursor      = (int) Math.round( ((double)this.cursor)            * ratio );
                    
                    // NOTE : (Wed, 30 Oct 2019 05:35:28 +0900)
                    // multiplying by ratio on `this.totalCursor` may not work as expected 
                    // but since changing tempo while recording is an improper action so
                    // we ignore the risk for now.
                    this.totalCursor = (int) Math.round( ((double)this.totalCursor)       * ratio );
                    
                    
                    // System.out.println( "after cursor: " + cursor );
                    // System.out.println( "lengthInFrame    : " + lengthInFrame );
                    // System.out.println( "prevLengthInFrame: " + prevLengthInFrame );
                    // this.lastLengthInFrame = (int) Math.round( ((double)this.lastLengthInFrame) * ratio );
                }
            }
        } catch ( JackException e ) {
            throw new MetroException(e);
        }
    }

    private long getTotalBufferLength() {
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
    
    /*
     * Lazy initializing field
     */
    private int cacheUpdateThreshold = -1;
    private static final int MIN_UPDATE_THRESHOLD = 256;
    private static final int MAX_UPDATE_THRESHOLD = 44100*4;
    
    
    @Override
    public void processBuffer( Metro metro, int barLengthInFrames) throws MetroException {
        synchronized ( metro.getMetroLock() ) { // << ADDED synchronided (Sun, 30 Sep 2018 11:45:13 +0900)
            super.processBuffer(metro, barLengthInFrames);
            
//          if ( this.buffers.size() < BUFFER_SIZE ) {
//              this.offerNewBuffer( metro, client, position );
//          }
            
//          MODIFIED >>> (Fri, 02 Aug 2019 16:52:08 +0900)
//          while ( getAccumulatedLength() < MARGIN_LENGTH * MARGIN_LENGTH ) {
//              this.offerNewBuffer( metro, client, position );
//          }
            
            int updateThreshold = cacheUpdateThreshold;
            if ( updateThreshold < 0 ) {
                double metroUpdateThreshold = metro.getUpdateSequenceThreshold();
                
                updateThreshold = (int)((double)barLengthInFrames * metroUpdateThreshold);
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
            
            while ( getTotalBufferLength() < updateThreshold ) {
                try {
                    this.offerNewBuffer( metro, barLengthInFrames );
                } catch (JackException e) {
                    throw new MetroException(e);
                }
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


    private void offerNewBuffer( Metro metro, int barLengthInFrames ) throws JackException {
        synchronized ( metro.getMetroLock() ) {
            
//          logInfo( "offerNewBuffer:" );
            if ( this.ending ) {
//              logInfo( "offerNewBuffer:ending (" + this.name  + ")");

                if ( this.endingDone ) {
                    if ( DEBUG )
                        logInfo( "offerNewBuffer(): endingDone is true" );
                    MetroEventBuffer receiver = MetroEventBuffer.create();
                    receiver.length( 1.0 );
                    receiver.prepare( barLengthInFrames, true );
                    this.buffers.offer( receiver );
                    
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
                                    metro.notifyTrackChange();
                                }
                            }
                        }
                    });
                    buf.length( this.endingLength  );
                    buf.prepare( barLengthInFrames, true );
                    this.buffers.offer( buf );
                }
                
            } else {
                // ??? is this really necessary? (Sun, 19 Apr 2020 09:48:14 +0900)
                // REMOVED (Sun, 19 Apr 2020 16:05:49 +0900) 
                // initialize the current thread
                // this.metro.getThreadInitializerCollection().initialize();

//              logInfo( "offerNewBuffer:normal (" + this.name  + ")");
                MetroEventBuffer buf = MetroEventBuffer.create();
                this.processBuffered( metro, buf );
                buf.prepare( barLengthInFrames, true );

                if ( DEBUG && ( buf.size() >0 ) )
                    logInfo( buf.dump("") );
                
                this.buffers.offer( buf );
                
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
            }
        }
    }
} 
