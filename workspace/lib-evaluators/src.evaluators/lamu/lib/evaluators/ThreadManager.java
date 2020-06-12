package lamu.lib.evaluators;

import lamu.lib.evaluators.AsyncThreadManager.AsyncThreadManagerThread;
import lamu.lib.threads.LamuThreadLocal;

public interface ThreadManager {
    public static final LamuThreadLocal<ThreadManager> threadLocal = new LamuThreadLocal<>();
    public static ThreadManager getCurrent(){
        return threadLocal.get();
    }
    public static void setCurrent( ThreadManager threadManager ){
        threadLocal.set( threadManager );
    }
    /**
     * See {@link AsyncThreadManagerThread#run() }
     */
    default void setCurrentThreadManager() {
        setCurrent(this);
    }
    void startThread( Runnable r );
    void interruptAllThreads();
}
