package metro;


import java.util.List;

public interface MetroTrackSynchronizer {
    long syncronizeTrack( Metro metro, MetroTrack track, List<MetroTrack> tracks, long measureLengthInFrames );
}
