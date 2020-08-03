package metro;


import java.util.List;

/**
 * This object represents how to synchronize the specific track. See {@link MetroTrackSelectorBasic}. 
 */
public interface MetroTrackSynchronizer {
    long syncronizeTrack( Metro metro, MetroTrack track, List<MetroTrack> tracks, long measureLengthInFrames );
}
