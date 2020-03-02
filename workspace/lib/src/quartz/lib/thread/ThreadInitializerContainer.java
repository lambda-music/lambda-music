package quartz.lib.thread;

public interface ThreadInitializerContainer<T> {
    ThreadInitializer<T> getThreadInitializer();
}
