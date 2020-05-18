package metro;

import java.util.List;

public interface MetroTrackSynchronizer {
    public static MetroTrackSynchronizer create(long delay ) {
        return new MetroTrackSynchronizer() {
            @Override
            public long syncronizeTrack(
                Metro metro, MetroSyncTrackAbstract track, 
                List<MetroTrack> tracks,
                long measureLengthInFrames) {
                return delay;
            }
        };
    }
    public static final MetroTrackSynchronizer IMMEDIATE = create(0);
    long syncronizeTrack( Metro metro, MetroSyncTrackAbstract track, List<MetroTrack> tracks, long measureLengthInFrames );
}
