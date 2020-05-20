package metro;

public interface MetroSynchronizedStopper {
    MetroTrackSynchronizer getStopSynchronizer();
    void setStopSynchronizer(MetroTrackSynchronizer stopSynchronizer);
}
