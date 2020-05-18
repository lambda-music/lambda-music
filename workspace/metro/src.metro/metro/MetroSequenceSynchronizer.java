package metro;

import java.util.List;

public interface MetroSequenceSynchronizer {
    public static MetroSequenceSynchronizer create(long delay ) {
        return new MetroSequenceSynchronizer() {
            @Override
            public long syncronizeTrack( Metro metro, MetroTrack track, List<MetroTrack> tracks, long measureLengthInFrames) {
                return delay;
            }
        };
    }
    public static final MetroSequenceSynchronizer IMMEDIATE = create(0);
    long syncronizeTrack( Metro metro, MetroTrack track, List<MetroTrack> tracks, long measureLengthInFrames );
}
