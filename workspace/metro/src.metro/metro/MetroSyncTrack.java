package metro;

import java.lang.invoke.MethodHandles;
import java.util.Collection;
import java.util.logging.Level;

import lamu.lib.log.Logger;

public abstract class MetroSyncTrack extends MetroTrack {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }
    static final boolean DEBUG = false;
    public MetroSyncTrack(Object name, Collection<Object> tags) {
        super(name, tags);
    }
    private transient MetroSyncType syncType = MetroSyncType.IMMEDIATE;
    private transient MetroSyncTrack syncTrack = null;
    private transient double syncOffset=0.0d;
    public MetroSyncType getSyncType() {
        return syncType;
    }
    public MetroSyncTrack  getSyncTrack() {
        return syncTrack;
    }
    public double getSyncOffset() {
        return syncOffset;
    }
    
    /**
     * 
     */
    public void reset() {
        syncType = null;
        syncTrack = null;
        syncOffset = 0.0d;
    }

    /**
     * @param syncType
     *            Specifying the way to synchronize with the syncTrack object. See
     *            {@link MetroSyncType}
     * @param syncTrack
     *            Specifying the track object to synchronize with.
     * @param syncOffset
     *            Specifying the distance from the track object with which is
     *            synchronized.
     * @return this object
     * 
     */
    public void setSyncStatus( MetroSyncType syncType, MetroSyncTrack syncTrack, double syncOffset ) {
        this.reset();
        this.syncType = syncType;
        this.syncTrack = syncTrack;
        this.syncOffset = syncOffset;
    }


    public abstract int getCursor();
    public abstract void setCursor(int cursor);
    public abstract int getLatestLengthInFrames(Metro metro);
    public abstract double getPosition(Metro metro);
    public abstract void removeGracefully();
    
    public static void setSyncStatus( Metro metro, MetroSyncTrack track, int barLengthInFrames) {
        int offset = (int) (-1.0d * track.getSyncOffset() * barLengthInFrames);

        switch ( track.getSyncType() ) {
            case IMMEDIATE :
            {
                track.setCursor( offset );
                if ( DEBUG )
                    logInfo( "prepare(immediate):" + track.getCursor() );
                if ( track.getSyncTrack() != null ) {
                    logWarn( "syncTrack was specified but the track was ignored because syncType was `immediate`." );
                }
            }
            break;
            case PARALLEL :
            {
                if ( track.getSyncTrack() == null ) {
                    track.setCursor( offset );
                    logWarn(  "`parallel` was specified but syncTrack was not specified; it was treated as immediate mode." );
                } else {
                    track.setCursor( track.getSyncTrack().getCursor() + offset );
                }

            }
            break;
            case SERIAL :
                if ( track.getSyncTrack() == null ) {
                    track.setCursor( offset );
                    logWarn( "`serial` was specified but syncTrack was not passed." );
                } else {
                    int length = track.getSyncTrack().getLatestLengthInFrames(metro);
                    if ( length < 0 ) {
                        track.setCursor( offset );
                        logWarn(  "`serial` was specified but track-length was not supported on the track; it was treated as immediate mode." );
                    } else {
                        track.setCursor( track.getSyncTrack().getCursor() - length + offset );
                    }

//                    synchronized ( track.getSyncTrack().getMetroTrackLock() ) {
//                        track.setCursor( 
//                                track.getSyncTrack().getCursor() - 
//                                track.getSyncTrack().getBuffers().peek().getLengthInFrames() + 
//                                offset );
//                    }
                }
                break;
            default :
                throw new RuntimeException( "Internal Error" ); // this won't occur.
        }
    }
}
