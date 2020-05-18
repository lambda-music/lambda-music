package metro;

import java.lang.invoke.MethodHandles;
import java.util.Collection;
import java.util.List;
import java.util.logging.Level;

import lamu.lib.log.Logger;

public abstract class MetroSyncTrack extends MetroTrack implements MetroSyncTrackAbstract {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }
    static final boolean DEBUG = false;

    private MetroTrackSynchronizer trackSynchronizer;
    public MetroTrackSynchronizer getTrackSynchronizer() {
        return trackSynchronizer;
    }
    public MetroSyncTrack(Object name, Collection<Object> tags, MetroTrackSynchronizer trackSynchronizer ) {
        super(name, tags);
        this.trackSynchronizer = trackSynchronizer;
    }
    
    private volatile boolean syncPrepared = false;
    public void resetSyncStatus() {
        this.syncPrepared = false;
    }

    /**
     * This method will be called only once by the Metro messaging thread when
     * MetroTrack is added to registered Track.
     * @param metro TODO
     * @param tracks TODO
     * @param measureLengthInFrames
     * @throws MetroException 
     */
    public void synchronizeTrack( Metro metro, List<MetroTrack> tracks, long measureLengthInFrames ) throws MetroException {
        if ( ! this.syncPrepared ) {
            this.syncPrepared = true;
            long positionInFrames = 
                this.trackSynchronizer.syncronizeTrack( metro, this, tracks, measureLengthInFrames );
            this.setCurrentPositionInFrames(metro, positionInFrames);
        }
    }
}
