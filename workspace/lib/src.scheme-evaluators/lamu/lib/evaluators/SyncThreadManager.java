package lamu.lib.evaluators;

public class SyncThreadManager implements ThreadManager {
    public static final SyncThreadManager SYNC = new SyncThreadManager();
    @Override
    public void startThread(Runnable r) {
        setCurrentThreadManager();
        r.run();
    }
    @Override
    public void interruptAllThreads() {
        throw new IllegalStateException("not supported");
    }
}