package metro;

import java.util.List;

public interface MetroMessage {
    void executeMessage( Metro metro, List<MetroTrack> tracks, long measureLengthInFrames );
}
