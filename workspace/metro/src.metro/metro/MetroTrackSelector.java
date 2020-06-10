package metro;

import java.util.Collections;
import java.util.List;

public interface MetroTrackSelector {
    void selectTracks( List<MetroTrack> currentTracks, List<MetroTrack> selectedTracks );
    
    /**
     * See {@link MetroTrackSelectorBasic} .
     * 
     * @param currentTracks
     * @param trackSelectors
     * @param selectedTracks TODO
     */
    public static void executeSelector( List<MetroTrackSelector> trackSelectors, List<MetroTrack> currentTracks, List<MetroTrack> selectedTracks ) {
        if ( trackSelectors == null )
            throw new NullPointerException( "trackSelector == null" );
        
        currentTracks = Collections.unmodifiableList(currentTracks);
        for ( MetroTrackSelector trackSelector : trackSelectors ) {
            trackSelector.selectTracks( currentTracks, selectedTracks );
        }
    }
    
    public static void executeSelector( Metro metro, List<MetroTrackSelector> trackSelectors, List<MetroTrack> selectedTracks ) {
        executeSelector( trackSelectors, metro.replicateAllTracks(), selectedTracks );
    }
    public static void resolveSelector( List<MetroTrackSelector> trackSelectors, List<MetroTrack> selectedTracks ) {
        executeSelector( trackSelectors, Collections.emptyList() , selectedTracks );
    }
}