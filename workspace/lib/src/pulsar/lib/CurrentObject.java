package pulsar.lib;

public final class CurrentObject<T> {
    private final ThreadLocal<T> threadLocal = new ThreadLocal<>();
    public void set( T current ) {
        threadLocal.set( current );
    }
    public T get() {
        T current = threadLocal.get();
        if ( current == null ) 
            throw new IllegalStateException();
        return current;
    }
    public static interface ThreadInitializable<T> {
        ThreadInitializer<T> getThreadInitializer();
    }
    public static final class ThreadInitializer<T> implements Runnable {
        CurrentObject<T> currentObject;
        T thisObject;
        public ThreadInitializer(CurrentObject<T> currentObject, T thisObject ) {
            super();
            this.currentObject = currentObject;
            this.thisObject = thisObject;
        }
        @Override
        public void run() {
            currentObject.set( thisObject );
        }
    }
}
