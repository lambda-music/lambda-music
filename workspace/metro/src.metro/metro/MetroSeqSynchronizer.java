package metro;

import java.util.List;

public interface MetroSeqSynchronizer {
    public static MetroSeqSynchronizer create(long delay ) {
        return new MetroSeqSynchronizer() {
            @Override
            public long syncronizeTrack( Metro metro, MetroTrack track, List<MetroTrack> tracks, long measureLengthInFrames) {
                return delay;
            }
        };
    }
    public static final MetroSeqSynchronizer IMMEDIATE = create(0);
    long syncronizeTrack( Metro metro, MetroTrack track, List<MetroTrack> tracks, long measureLengthInFrames );
}
