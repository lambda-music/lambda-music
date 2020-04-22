package lamu.lib.evaluators;

public interface ThreadManager {
    public static final ThreadLocal<ThreadManager> threadLocal = new ThreadLocal<>();
    public static ThreadManager getCurrent(){
        return threadLocal.get();
    }
    public static void setCurrent( ThreadManager threadManager ){
        threadLocal.set( threadManager );
    }
    default void setCurrentThreadManager() {
        setCurrent(this);
    }
    void startThread( Runnable r );
    void interruptAllThreads();
}
