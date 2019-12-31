package kawapad;

public interface ThreadManager {
    
    void startThread(Runnable r);
    
    void interruptAllThreads();
    
}