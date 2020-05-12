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
    static final boolean DEBUG = true;
    public MetroSyncTrack(Object name, Collection<Object> tags, MetroSyncType syncType, MetroSyncTrack syncTrack, double syncOffset ) {
        super(name, tags);
        this.syncType = syncType;
        this.syncTrack = syncTrack;
        this.syncOffset = syncOffset;
    }
    private volatile MetroSyncType syncType = MetroSyncType.IMMEDIATE;
    private volatile MetroSyncTrack syncTrack = null;
    private volatile double syncOffset=0.0d;
    public MetroSyncType getSyncType() {
        return syncType;
    }
    public MetroSyncTrack  getSyncTrack() {
        return syncTrack;
    }
    public double getSyncOffset() {
        return syncOffset;
    }

    private volatile long lastBarLengthInFrames = -1;
    private volatile boolean syncPrepared = false;

    /**
     * 
     */
    public void resetSyncStatus() {
        this.syncPrepared = false;
        this.syncType = MetroSyncType.IMMEDIATE;
        this.syncTrack = null;
        this.syncOffset = 0.0d;
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
        this.resetSyncStatus();
        this.syncType = syncType;
        this.syncTrack = syncTrack;
        this.syncOffset = syncOffset;
    }


    // (Tue, 05 May 2020 18:58:13 +0900) This method was formerly getCursor() 
    public abstract long getCurrentPositionInFrames(Metro metro);
    // (Tue, 05 May 2020 18:58:13 +0900) This method was formerly setCursor() 
    public abstract void setCurrentPositionInFrames(Metro metro, long position);
    // (Tue, 05 May 2020 18:58:13 +0900) This method was formerly getLatestLengthInFrames() 
    public abstract long getCurrentLengthInFrames(Metro metro);
    // (Tue, 05 May 2020 18:58:13 +0900) This method was formerly getPosition() 
    public abstract double getPosition(Metro metro);

    // Created (Thu, 07 May 2020 03:14:15 +0900)
    public abstract void prepareSyncStatus(Metro metro, long barLengthInFrames) throws MetroException;
    // Created (Thu, 07 May 2020 03:14:15 +0900)
    public abstract void reprepareSyncStatus(Metro metro, long barLengthInFrames) throws MetroException;
    

    @Override
    public void processBuffer( Metro metro, long barLengthInFrames) throws MetroException {
        synchronized ( metro.getMetroLock() ) { // << ADDED synchronided (Sun, 30 Sep 2018 11:45:13 +0900)
            if ( barLengthInFrames != lastBarLengthInFrames ) {
                lastBarLengthInFrames = barLengthInFrames;
                if ( ! syncPrepared ) {
                    syncPrepared = true;
                    prepareSyncStatus(metro, barLengthInFrames);
                } else {
                    reprepareSyncStatus(metro, barLengthInFrames );
                }
            }
        }
    }

    public static void prepareSyncStatus( Metro metro, MetroSyncTrack track, long barLengthInFrames) {
        MetroSyncType syncType   = track.getSyncType();
        MetroSyncTrack syncTrack = track.getSyncTrack(); 
        double syncOffset        = track.getSyncOffset();
        long offset = (long) ((1.0d * syncOffset) * barLengthInFrames);
        
        if ( DEBUG ) {
            
            logInfo( "===prepareSyncStatus===" );
            logInfo( "syncType:" + syncType );
            logInfo( "syncTrack:" + syncTrack );
            logInfo( "syncOffset:" + syncOffset );
            if(syncTrack !=null)
                logInfo( "syncTrack.getCurrentPositionInFrames(metro)" + syncTrack.getCurrentPositionInFrames(metro) );
        }

        switch ( syncType ) {
            case IMMEDIATE :
            {
                track.setCurrentPositionInFrames( metro, offset );
                if ( DEBUG )
                    logInfo( "prepare(immediate):" + track.getCurrentPositionInFrames(metro) );
                if ( syncTrack != null ) {
                    logWarn( "syncTrack was specified but the track was ignored because syncType was `immediate`." );
                }
            }
            break;
            case PARALLEL :
            {
                if ( DEBUG )
                    logInfo( "prepare(parallel):" + track.getCurrentPositionInFrames(metro) );

                if ( syncTrack == null ) {
                    track.setCurrentPositionInFrames( metro, offset );
                    logWarn(  "`parallel` was specified but syncTrack was not specified; it was treated as immediate mode." );
                } else {
                    track.setCurrentPositionInFrames( metro, syncTrack.getCurrentPositionInFrames(metro) + offset );
                }

            }
            break;
            case SERIAL :
                if ( DEBUG )
                    logInfo( "prepare(SERIAL):" + track.getCurrentPositionInFrames(metro) );

                if ( syncTrack == null ) {
                    track.setCurrentPositionInFrames( metro, offset );
                    logWarn( "`serial` was specified but syncTrack was not passed." );
                } else {
                    long length = syncTrack.getCurrentLengthInFrames(metro);
                    if ( length < 0 ) {
                        track.setCurrentPositionInFrames( metro, offset );
                        logWarn(  "`serial` was specified but track-length was not supported on the track; it was treated as immediate mode." );
                    } else {
                        track.setCurrentPositionInFrames( metro, syncTrack.getCurrentPositionInFrames(metro) - length + offset );
                    }

//                    synchronized ( syncTrack().getMetroTrackLock() ) {
//                        track.setCurrentPositionInFrames( 
//                                syncTrack().getCurrentPositionInFrames() - 
//                                syncTrack().getBuffers().peek().getLengthInFrames() + 
//                                offset );
//                    }
                }
                break;
            default :
                throw new RuntimeException( "Internal Error" ); // this won't occur.
        }
        if ( DEBUG ) {
            logInfo("");
        }
    }
}
