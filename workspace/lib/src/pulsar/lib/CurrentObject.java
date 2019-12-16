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
}
