package metro;

import java.util.List;

public interface MetroMessage {
    void executeMessage( Metro metro, List<MetroTrack> currentTracks, List<MetroTrack> registeringTracks, List<MetroTrack> finalizingTracks, List<MetroTrack> unregisteringTracks, long measureLengthInFrames );
}
