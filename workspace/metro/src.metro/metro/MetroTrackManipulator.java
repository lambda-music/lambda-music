package metro;

import java.util.List;

public interface MetroTrackManipulator {
    void manipulateTracks(
        List<MetroTrack> currentTracks,
        List<MetroTrack> registeringTracks,
        List<MetroTrack> removingTracks,
        List<MetroTrack> unregisteringTracks);
}
