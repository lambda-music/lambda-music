package metro;

import java.util.List;

public interface MetroReft extends MetroVerb {
    void referTracks( List<MetroTrackSelector> trackSelectors, List<MetroTrack> selectedTracks );
}
