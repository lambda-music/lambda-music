package metro;

import java.util.Collection;

public interface MetroRemt extends MetroMant {
    default void removeFormerTrack(Collection<MetroTrack> tracks, MetroTrackSynchronizer trackSynchronizer ) {
        removeTrack( MetroTrackSelectorBasic.correspondingNamedTrack(tracks), trackSynchronizer );
    }

    default void removeTrack( Collection<MetroTrack> tracks, MetroTrackSynchronizer trackSynchronizer )  {
        removeTrack( MetroTrackSelectorBasic.constant(tracks), trackSynchronizer );
    }

    /**
     * 
     * @param selector
     * @param synchronizer
     *    Set the synchronizer to the selected tracks before the operation unless it is null.
     *    
     */
    default void removeTrack( MetroTrackSelector selector, MetroTrackSynchronizer synchronizer ) {
        manipulateTrack( MetroTrackManipulatorBasic.removing( selector ) );
    };
}
