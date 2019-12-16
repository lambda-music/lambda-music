package pulsar;

public interface PulsarApplicationComponent<T> {
    void requesetInit();
    void requestShutdown();
}
