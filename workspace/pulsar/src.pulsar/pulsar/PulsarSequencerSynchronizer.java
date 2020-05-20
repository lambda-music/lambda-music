package pulsar;

import java.util.List;

import gnu.lists.LList;
import gnu.mapping.Procedure;
import metro.Metro;
import metro.MetroTrackSynchronizer;
import metro.MetroTrack;

public class PulsarSequencerSynchronizer implements MetroTrackSynchronizer {
    public static final MetroTrackSynchronizer create( Procedure procedure ) {
        return new PulsarSequencerSynchronizer( procedure );
    }
    final Procedure procedure;
    public PulsarSequencerSynchronizer(Procedure procedure) {
        super();
        this.procedure = procedure;
    }

    @Override
    public long syncronizeTrack(Metro metro, MetroTrack track, List<MetroTrack> tracks, long measureLengthInFrames) {
        try {
            return (long) procedure.apply4( metro, track, LList.makeList( tracks ), measureLengthInFrames  );
        } catch (Throwable e) {
            throw new RuntimeException(e);
        }
    }

}
