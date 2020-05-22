package metro;

import java.util.List;

public interface MetroTrackManipulator {
    void createTracks( List<MetroTrack> currentTracks, List<MetroTrack> registeringTracks, List<MetroTrack> unregisteringTracks );
}
