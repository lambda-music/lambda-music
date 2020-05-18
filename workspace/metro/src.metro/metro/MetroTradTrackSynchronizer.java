package metro;

import java.lang.invoke.MethodHandles;
import java.util.List;
import java.util.logging.Level;

import lamu.lib.log.Logger;

public class MetroTradTrackSynchronizer implements MetroTrackSynchronizer {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }
    static final boolean DEBUG = false;

    public static MetroTrackSynchronizer create( MetroSyncType syncType, MetroTrack syncTrack,  double syncOffset) {
        return new MetroTradTrackSynchronizer(syncType, syncTrack, syncOffset);
    }
    MetroSyncType syncType   ;
    MetroSyncTrackAbstract syncTrack ; 
    double syncOffset        ;
    private MetroTradTrackSynchronizer(MetroSyncType syncType, MetroSyncTrackAbstract syncTrack, double syncOffset) {
        this.syncType = syncType;
        this.syncTrack = syncTrack;
        this.syncOffset = syncOffset;
    }
    private MetroTradTrackSynchronizer(MetroSyncType syncType, MetroTrack syncTrack, double syncOffset) {
        if ( syncTrack != null &&  !(syncTrack instanceof MetroSyncTrackAbstract) )
            throw new IllegalArgumentException( );
        this.syncType = syncType;
        this.syncTrack = (MetroSyncTrackAbstract)syncTrack;
        this.syncOffset = syncOffset;
    }
    @Override
    public long syncronizeTrack(
        Metro metro, 
        MetroSyncTrackAbstract track, 
        List<MetroTrack> tracks,
        long measureLengthInFrames) 
    {
        return MetroTradTrackSynchronizer.synchronizeTrack( metro, track, tracks, measureLengthInFrames, syncType, syncTrack, syncOffset );
    }
    
    public static long synchronizeTrack( Metro metro, MetroSyncTrackAbstract track, List<MetroTrack> tracks, long measureLengthInFrames, 
        MetroSyncType syncType, MetroSyncTrackAbstract syncTrack, double syncOffset ) 
    {
        long result;
        long offset = (long) ((1.0d * syncOffset) * measureLengthInFrames);

        if ( DEBUG ) {

            logInfo( "===synchronizeTrack===" );
            logInfo( "syncType:" + syncType );
            logInfo( "syncTrack:" + syncTrack );
            logInfo( "syncOffset:" + syncOffset );
            if(syncTrack !=null)
                logInfo( "syncTrack.getCurrentPositionInFrames(metro)" + syncTrack.getCurrentPositionInFrames(metro) );
        }

        switch ( syncType ) {
        case IMMEDIATE :
        {
            result = offset;
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

            result = offset;
            if ( syncTrack == null ) {
                result = offset;
                logWarn(  "`parallel` was specified but syncTrack was not specified; it was treated as immediate mode." );
            } else {
                result = syncTrack.getCurrentPositionInFrames(metro) + offset;
            }

        }
        break;
        case SERIAL :
            if ( DEBUG )
                logInfo( "prepare(SERIAL):" + track.getCurrentPositionInFrames(metro) );

            if ( syncTrack == null ) {
                result = offset;
                logWarn( "`serial` was specified but syncTrack was not passed." );
            } else {
                long length = syncTrack.getCurrentLengthInFrames(metro);
                if ( length < 0 ) {
                    result = offset;
                    logWarn(  "`serial` was specified but track-length was not supported on the track; it was treated as immediate mode." );
                } else {
                    result = syncTrack.getCurrentPositionInFrames(metro) - length + offset;
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
        return result;
    }
}