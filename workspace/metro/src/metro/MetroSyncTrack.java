package metro;

import java.lang.invoke.MethodHandles;
import java.util.logging.Level;

import lamu.lib.log.PulsarLogger;

public interface MetroSyncTrack {
    static final PulsarLogger LOGGER = PulsarLogger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }
    static final boolean DEBUG = false;

    int getCursor();
    void setCursor(int cursor);
    MetroSyncType getSyncType();
    MetroSyncTrack getSyncTrack();
    double getSyncOffset();
    int getLatestLengthInFrames();
    
    public static void setSyncStatus( MetroSyncTrack track, int barLengthInFrames) {
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
                    int length = track.getSyncTrack().getLatestLengthInFrames();
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
