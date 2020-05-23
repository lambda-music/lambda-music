package metro;

public interface MetroSynchronizedStopper {
    MetroTrackSynchronizer getStopSynchronizer();
    void setStopSynchronizer(MetroTrackSynchronizer stopSynchronizer);
    void stop(Metro metro, MetroTrack track);
}
