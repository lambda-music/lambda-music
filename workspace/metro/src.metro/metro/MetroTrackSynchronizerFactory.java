package metro;

public interface MetroTrackSynchronizerFactory {
    public abstract MetroTrackSynchronizer createSynchronizer( MetroTrackSelector syncTrack, double syncOffset );
}
