package pulsar.lib;

public interface ThreadInitializerContainer<T> {
    ThreadInitializer<T> getThreadInitializer();
}
