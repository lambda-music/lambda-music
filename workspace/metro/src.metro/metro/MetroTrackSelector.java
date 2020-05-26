package metro;

import java.util.ArrayList;
import java.util.List;

public interface MetroTrackSelector {
    void selectTracks( List<MetroTrack> currentTracks, List<MetroTrack> selectedTracks );

    
    /**
     * See {@link MetroTrackSelectorBasic} .
     * 
     * @param tracks
     * @param trackSelector
     * @return
     */
    public static List<MetroTrack> doSelectTracks( List<MetroTrack> tracks, MetroTrackSelector trackSelector ) {
        if ( trackSelector == null )
            throw new NullPointerException( "selector == null" );
        ArrayList<MetroTrack> result = new ArrayList<>();
        trackSelector.selectTracks( tracks, result );
        return result;
    }
}