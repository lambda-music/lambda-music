package metro;

import java.util.Arrays;
import java.util.Collection;

public interface MetroRemt extends MetroMant {
    default void removeFormerTrack(Collection<MetroTrack> tracks, MetroTrackSynchronizer trackSynchronizer ) {
        removeTrack( MetroTrackSelectorBasic.correspondingNamedTrack(tracks), trackSynchronizer );
    }

    default void removeTrack( Collection<MetroTrack> tracks, MetroTrackSynchronizer trackSynchronizer )  {
        removeTrack( MetroTrackSelectorBasic.trackConstant(tracks), trackSynchronizer );
    }

    /**
     * 
     * @param trackSelector
     * @param trackSynchronizer
     *    Set the synchronizer to the selected tracks before the operation unless it is null.
     *    
     */
    default void removeTrack( MetroTrackSelector trackSelector, MetroTrackSynchronizer trackSynchronizer ) {
        manipulateTrack( Arrays.asList( MetroTrackManipulatorBasic.removing(trackSelector)));
    };
}
