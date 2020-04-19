package lamu.lib.thread;

public interface ThreadInitializerContainer<T> {
    ThreadInitializer<T> getThreadInitializer();
}
