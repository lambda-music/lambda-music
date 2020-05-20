package metro;

public interface MetroSynchronizedStarter {
    MetroTrackSynchronizer getStartSynchronizer();
    void setStartSynchronizer(MetroTrackSynchronizer startSynchronizer);
}
