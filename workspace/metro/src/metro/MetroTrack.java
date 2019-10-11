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
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.jaudiolibs.jnajack.JackClient;
import org.jaudiolibs.jnajack.JackException;
import org.jaudiolibs.jnajack.JackPosition;

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
public class MetroTrack implements MetroLock, EventListenable {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }


    public enum SyncType {
        IMMEDIATE, PARALLEL, SERIAL;
        public static SyncType toSyncType( String value ) {
            if ( value == null ) throw new IllegalArgumentException();
            if ( "para".equals( value ) )
                return PARALLEL;
            else if ( "p".equals( value ) )
                return SyncType.PARALLEL;
            else if ( "seri".equals( value ) )
                return SyncType.SERIAL;
            else if ( "s".equals( value ) )
                return SyncType.SERIAL;
            else if ( "imme".equals( value ) )
                return SyncType.IMMEDIATE;
            else if ( "i".equals( value ) )
                return SyncType.IMMEDIATE;
            else if ( "immediately".equals( value ) )
                return SyncType.IMMEDIATE;
            else
                return SyncType.valueOf( value.toUpperCase() );
        }
    }
    
    
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

    /*
     * This value is only for debugging purpose. 
     */
    final int id = (int) (Math.random()* Integer.MAX_VALUE);
    
    /**
     * 
     */
    private final Metro metro;
    /**
     * Note that the String object which is stored in name field must be interned.  
     */
    protected final Object name;
    protected final Collection<Object> tags;
    protected final MetroSequence sequence;

    protected transient boolean prepared = false;
    protected transient boolean enabled = true;

//  private static final int BUFFER_SIZE = 2;
// MODIFIED >>> (Mon, 08 Jul 2019 20:29:49 +0900)
//  private static final double MARGIN_LENGTH = 2;   
//  private static final double MARGIN_LENGTH = 9;   
// MODIFIED <<< 


    private transient SyncType syncType;
    private transient MetroTrack syncTrack;
    private transient double syncOffset=0.0d;
    private BlockingQueue<MetroEventBuffer> buffers = new LinkedBlockingQueue<>();
    protected transient int cursor = 0;
    protected transient int lastLengthInFrames = 0;
    protected transient int lastAccumulatedLength = 0;
    
    transient boolean ending = false;
    transient boolean endingDone = false;
    transient double endingLength = 0;
    transient Runnable endingProc = null;
    
    
    void reset() {
        prepared = false;
        enabled = true;
        
        syncType = null;
        syncTrack = null;
        syncOffset = 0.0d;
        
        buffers.clear();
        cursor = 0;
        lastLengthInFrames = 0;
        lastAccumulatedLength = 0;
        
        ending = false;
        endingDone = false;
        endingLength = 0;
        endingProc = null;
        
//        eventListenerable.clearEventListeners();
    }
    
    public static final Object EVENT_PREPARED = "prepared"; 
    
    protected final EventListenable eventListenerable = new EventListenable.Default( this );
    @Override
    public void clearEventListeners() {
        synchronized ( this.getMetroLock() ) {
            eventListenerable.clearEventListeners();
        }
    }
    @Override
    public void addEventListener(Object type, Listener listener) {
        synchronized ( this.getMetroLock() ) {
            eventListenerable.addEventListener( type, listener );
        }
    }
    @Override
    public void removeEventListener(Listener listener) {
        synchronized ( this.getMetroLock() ) {
            eventListenerable.removeEventListener( listener );
        }
    }
    @Override
    public void invokeEventListener(Object type) {
        synchronized ( this.getMetroLock() ) {
            eventListenerable.invokeEventListener( type );
        }
    }
    /**
     * Create a MetroTrack object with default synchronizing status.
     *
     * This constructor is <code>public</code> but this is usually called only from
     * {@link Metro#createTrack(String, Collection, MetroSequence, SyncType, MetroTrack, double)}
     * and users should not call this constructor directory. Though it is still left public
     * for further hacking.
     * 
     * @param metro
     *            Specifying the parent metro object.
     * @param name
     *            Specifying the identifier of the track.
     * @param tags
     *            Specifying the tag strings. This could be null and treated as an
     *            empty set.
     * @param sequence
     *            Specifying the sequence object to play.
     */
    MetroTrack( Metro metro, Object name, Collection<Object> tags, MetroSequence sequence ) {
        if ( name == null )
            this.name = createUniqueTrackName();
        else
            this.name = checkName( name );
        
        
        if ( tags == null )
            this.tags = Collections.EMPTY_LIST;
        else
            this.tags = tags;

        this.metro = metro;
        this.sequence = sequence;
        
        this.syncType = SyncType.IMMEDIATE;
        this.syncTrack = null;
        this.syncOffset = 0.0d;
    }
    
    private static Object uniqueTrackNameLock = new Object();
    private static Object getUniqueTrackNameLock() {
        // TODO COUNTERMEASURE_FOR_LOCKING (Mon, 23 Sep 2019 08:33:32 +0900)
        return uniqueTrackNameLock;
    }
    private static transient int uniqueTrackNameCounter = 0;
    private static String createUniqueTrackName() {
        synchronized ( getUniqueTrackNameLock() ) {
            return "track-" + ( uniqueTrackNameCounter ++ ); 
        }
    }
    
    private static Object checkName( Object name ) {
        if ( name instanceof String ) {
            return ((String)name).intern();
        } else {
            return name;
        }
    }
    
    public Object getMetroTrackLock() {
        // TODO COUNTERMEASURE_FOR_LOCKING (Mon, 23 Sep 2019 08:33:32 +0900)
        return this.metro.getMetroLock();
//        return buffers;
    }

    @Override
    public Object getMetroLock() {
        // TODO COUNTERMEASURE_FOR_LOCKING (Mon, 23 Sep 2019 08:33:32 +0900)
        return this.metro.getMetroLock();
    }
    
    public MetroSequence getSequence() {
        return this.sequence;
    }
    public Object getTrackName() {
        return name;
    }
    public Collection<Object> getTrackTags() {
        return tags;
    }
    
    public boolean isTrackEnabled() {
        return enabled;
    }
    public void setTrackEnabled(boolean enabled) {
        this.enabled = enabled;
    }
    public void removeGracefully() {
        if ( this.ending ) {
            logWarn( "removeGracefully was called; but the track is already removed." );
        } else {
            logInfo( "removeGracefully was called; the ending flag is set." );
            this.ending = true;
        }
    }
    public double getTrackPosition() {
        if ( lastLengthInFrames < 0 || cursor < 0) {
            return 0;
        } else {
            return (double)( cursor - lastAccumulatedLength + lastLengthInFrames )  / (double)lastLengthInFrames;
        }
    }
    
    /**
     * @param syncType
     *            Specifying the way to synchronize with the syncTrack object. See
     *            {@link SyncType}
     * @param syncTrack
     *            Specifying the track object to synchronize with.
     * @param syncOffset
     *            Specifying the distance from the track object with which is
     *            synchronized.
     * @return this object
     * 
     */
    public MetroTrack setSyncStatus( SyncType syncType, MetroTrack syncTrack, double syncOffset ) {
        this.reset();
        this.prepared = false;
        this.syncType = syncType;
        this.syncTrack = syncTrack;
        this.syncOffset = syncOffset;
        return this;
    }
    public SyncType getSyncType() {
        return syncType;
    }
    public MetroTrack getSyncTrack() {
        return syncTrack;
    }
    public double getSyncOffset() {
        return syncOffset;
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

    static int BUFFER_REMOVAL_STRATEGY = 3;

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
    protected void progressCursor( int nframes, List<MetroAbstractMidiEvent> result ) throws JackException {
        synchronized ( this.getMetroTrackLock() ) {
            this.metro.clearAllPorts();

            int currentCursor = this.cursor;
            int nextCursor = currentCursor + nframes;
            
            // This keeps negative offset value for the current cursor position. 
            int cursorOffset = 0;

            for ( Iterator<MetroEventBuffer> ibuf = this.buffers.iterator(); ibuf.hasNext(); ) {
                MetroEventBuffer buf = ibuf.next();

                int actualCursor     = currentCursor - cursorOffset;
                int actualNextCursor = nextCursor    - cursorOffset;
                
                boolean found= false;
                for ( Iterator<MetroEvent> ie = buf.iterator(); ie.hasNext();  ) {
                    MetroEvent e = ie.next();
                    
                    if ( e.between( actualCursor, actualNextCursor ) ) {
                        found = true;
                        e.process( metro, actualCursor, actualNextCursor, nframes, result );
                    } else {
                        if ( found ) // SEE COMMENT_A (Fri, 02 Aug 2019 19:20:40 +0900)
                            break;
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
     * 
     * @param barInFrames
     */
    void prepare( int barInFrames ) {
        if ( prepared ) 
            throw new IllegalStateException();
        this.prepared = true;
        
        int offset = (int) (-1.0d * this.syncOffset * barInFrames);

        switch ( this.syncType ) {
            case IMMEDIATE :
            {
                this.cursor = offset;
                if ( DEBUG )
                    logInfo( "prepare(immediate):" + this.cursor );
                if ( this.syncTrack != null ) {
                    Logger.getLogger( Metro.class.getName()).log(Level.WARNING, "syncTrack was passed but ignored by the process because syncType was `immediate`." );
                }
            }
            break;
            case PARALLEL :
            {
                if ( this.syncTrack == null ) {
                    this.cursor = offset;
                    Logger.getLogger( Metro.class.getName()).log(Level.WARNING, "`parallel` was specified but syncTrack was not passed." );
                } else {
                    this.cursor = this.syncTrack.cursor + offset ;
                }

            }
            break;
            case SERIAL :
                if ( this.syncTrack == null ) {
                    this.cursor = offset;
                    Logger.getLogger( Metro.class.getName()).log(Level.WARNING, "`serial` was specified but syncTrack was not passed." );
                } else {
                    synchronized ( this.syncTrack.getMetroTrackLock() ) {
                        this.cursor =
                                this.syncTrack.cursor - 
                                this.syncTrack.buffers.peek().getLengthInFrames() + 
                                offset;
                    }
                }
                break;
            default :
                throw new RuntimeException( "Internal Error" ); // this won't occur.
        }
        
        eventListenerable.invokeEventListener( EVENT_PREPARED );
    }

    void reprepare( Metro metro, JackClient client, JackPosition position, 
            double prevBeatsPerMinute, double beatsPerMinute ) throws JackException 
    {
        synchronized ( this.getMetroTrackLock() ) {
            int prevLengthInFrame = -1;
            int lengthInFrame = -1;
            {
                MetroEventBuffer headBuffer = this.buffers.peek();
                if ( headBuffer != null )
                    prevLengthInFrame = headBuffer.getLengthInFrames();
            }
                
            // double ratio = magnifyCursorPosition( prevBeatsPerMinute, beatsPerMinute );
            for ( MetroEventBuffer buffer : this.buffers ) {
                buffer.prepare(metro, client, position, false);
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
                this.cursor = (int) Math.round( ((double)this.cursor)            * ratio );
                // System.out.println( "after cursor: " + cursor );
                // System.out.println( "lengthInFrame    : " + lengthInFrame );
                // System.out.println( "prevLengthInFrame: " + prevLengthInFrame );
                // this.lastLengthInFrame = (int) Math.round( ((double)this.lastLengthInFrame) * ratio );
            }
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
            logInfo( "getTotalBufferLength(" + name + "):(count:" + this.buffers.size() + ") " + total );
        return total;
    }
    
    /*
     * Lazy initializing field
     */
    private int cacheUpdateThreshold = -1;
    private static final int MIN_UPDATE_THRESHOLD = 256;
    private static final int MAX_UPDATE_THRESHOLD = 44100*4;
    protected  void checkBuffer( Metro metro, JackClient client, JackPosition position, int barInFrames) throws JackException {
        synchronized ( this.getMetroTrackLock() ) { // << ADDED synchronided (Sun, 30 Sep 2018 11:45:13 +0900)
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
                
                updateThreshold = (int)((double)barInFrames * metroUpdateThreshold);
                String message = "" + updateThreshold +"=" + barInFrames + "*" + metroUpdateThreshold +" (updateThreshold = barInFrames * metroUpdateThreshold)";
                
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
                logInfo( "checkBuffer(" + name + "):thre:" + updateThreshold  );
            
            while ( getTotalBufferLength() < updateThreshold ) {
                this.offerNewBuffer( metro, client, position );
            }
//          MODIFIED <<< (Fri, 02 Aug 2019 16:52:08 +0900)
        }
    }
    
    protected  void clearBuffer() {
        synchronized ( this.getMetroTrackLock() ) {
            this.buffers.clear();
            this.cursor =0;
        }
    }
//  public void resetBuffer() {
//      synchronized ( this.getMetroTrackLock() ) {
//          this.buffers.clear();
//          this.cursor =0;
//      }
//  }

    private void offerNewBuffer( Metro metro, JackClient client, JackPosition position ) throws JackException {
        synchronized ( this.getMetroTrackLock() ) {
//          logInfo( "offerNewBuffer:" );
            if ( this.ending ) {
//              logInfo( "offerNewBuffer:ending (" + this.name  + ")");

                if ( this.endingDone ) {
                    if ( DEBUG )
                        logInfo( "offerNewBuffer(): endingDone is true" );
                    MetroEventBuffer buf = new MetroEventBuffer();
                    buf.setLength( 1.0 );
                    buf.prepare( metro, client, position, true );
                    this.buffers.offer( buf );
                    
                } else {
                    if ( DEBUG )
                        logInfo( "offerNewBuffer(" + name + ") setting true endingDone " );
                    this.endingDone = true;

                    MetroEventBuffer buf = new MetroEventBuffer();
                    buf.exec( this.endingLength , new Runnable() {
                        @Override
                        public void run() {
                            if ( DEBUG )
                                logInfo( "offerNewBuffer(" +name + ") UNREGISTER THIS" );
                            synchronized ( metro.getMetroLock() ) {
                                try {
                                    metro.unregisterTrack( MetroTrack.this );
                                } finally {
                                    metro.notifyTrackChange();
                                }
                            }
                        }
                    });
                    buf.setLength( this.endingLength  );
                    buf.prepare( metro, client, position, true );
                    this.buffers.offer( buf );
                }
                
            } else {
//              logInfo( "offerNewBuffer:normal (" + this.name  + ")");
                MetroEventBuffer buf = new MetroEventBuffer();
                boolean result = this.sequence.processBuffered( metro, this, buf );
                buf.prepare( metro, client, position, true );

                if ( DEBUG && ( buf.size() >0 ) )
                    buf.dump();
                
                this.buffers.offer( buf );
                
                if ( result ) {
                    if ( DEBUG )
                        logInfo( "offerNewBuffer(" +name + ") CONTINUE" );
                } else {
                    if ( DEBUG )
                        logInfo( "offerNewBuffer(" +name + ") ENDING started");
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
        if ( false ) {
            if ( obj instanceof MetroTrack ) {
                return  this.name ==     ((MetroTrack)obj).name ||
                        this.name.equals(((MetroTrack)obj).name );
            } else {
                return false;
            }
        } else {
            return super.equals( obj );
        }
    }
    @Override
    public int hashCode() {
        if ( false ) {
            return this.name.hashCode() * 2;
        } else {
            return super.hashCode();
        }
    }
    @Override
    public String toString() {
        return "#" + MetroTrack.class.getSimpleName() + ":" + this.name + "#";
    }
} 
