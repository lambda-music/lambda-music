package metro;

import java.lang.invoke.MethodHandles;
import java.util.List;
import java.util.logging.Level;

import lamu.lib.log.Logger;

public class MetroTradSequenceSynchronizer implements MetroSequenceSynchronizer {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }
    static final boolean DEBUG = false;

    public static MetroSequenceSynchronizer create( MetroSyncType syncType, MetroTrack syncTrack,  double syncOffset) {
        return new MetroTradSequenceSynchronizer(syncType, syncTrack, syncOffset);
    }
    MetroSyncType syncType   ;
    MetroTrack syncTrack ; 
    double syncOffset        ;
    private MetroTradSequenceSynchronizer(MetroSyncType syncType, MetroTrack syncTrack, double syncOffset) {
        this.syncType = syncType;
        this.syncTrack = syncTrack;
        this.syncOffset = syncOffset;
    }
    @Override
    public long syncronizeTrack(
        Metro metro, 
        MetroTrack track, 
        List<MetroTrack> tracks,
        long measureLengthInFrames) 
    {
        return MetroTradSequenceSynchronizer.synchronizeTrack( metro, track, tracks, measureLengthInFrames, syncType, syncTrack, syncOffset );
    }
    
    public static long synchronizeTrack( 
        Metro metro,
        MetroTrack track, 
        List<MetroTrack> tracks, 
        long measureLengthInFrames,
        MetroSyncType syncType,
        MetroTrack syncTrack,
        double syncOffset ) 
    {
        MetroSyncSequence seq = (MetroSyncSequence)track.getSequence();
        MetroSyncSequence syncSeq = syncTrack == null ? null : (MetroSyncSequence)syncTrack.getSequence();
        
        long result;
        long offset = (long) ((1.0d * syncOffset) * measureLengthInFrames);

        if ( DEBUG ) {

            logInfo( "===synchronizeTrack===" );
            logInfo( "syncType:" + syncType );
            logInfo( "syncSeq:" + syncSeq );
            logInfo( "syncOffset:" + syncOffset );
            if(syncSeq !=null)
                logInfo( "syncTrack.getCurrentPositionInFrames(metro)" + syncSeq.getCurrentPositionInFrames(metro) );
        }

        switch ( syncType ) {
        case IMMEDIATE :
        {
            result = offset;
            if ( DEBUG )
                logInfo( "prepare(immediate):" + seq.getCurrentPositionInFrames(metro) );
            if ( syncSeq != null ) {
                logWarn( "syncTrack was specified but the track was ignored because syncType was `immediate`." );
            }
        }
        break;
        case PARALLEL :
        {
            if ( DEBUG )
                logInfo( "prepare(parallel):" + seq.getCurrentPositionInFrames(metro) );

            result = offset;
            if ( syncSeq == null ) {
                result = offset;
                logWarn(  "`parallel` was specified but syncTrack was not specified; it was treated as immediate mode." );
            } else {
                result = syncSeq.getCurrentPositionInFrames(metro) + offset;
            }

        }
        break;
        case SERIAL :
            if ( DEBUG )
                logInfo( "prepare(SERIAL):" + seq.getCurrentPositionInFrames(metro) );

            if ( syncSeq == null ) {
                result = offset;
                logWarn( "`serial` was specified but syncTrack was not passed." );
            } else {
                long length = syncSeq.getCurrentLengthInFrames(metro);
                if ( length < 0 ) {
                    result = offset;
                    logWarn(  "`serial` was specified but track-length was not supported on the track; it was treated as immediate mode." );
                } else {
                    result = syncSeq.getCurrentPositionInFrames(metro) - length + offset;
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