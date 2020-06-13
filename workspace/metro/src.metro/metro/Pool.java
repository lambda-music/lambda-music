package metro;

import java.util.ArrayDeque;

public abstract class Pool<T> {
    protected abstract T create();
    private final ArrayDeque<T> queue = new ArrayDeque<T>();
    public Pool( int initialSize ) {
        for ( int i=0; i<initialSize; i++ ) {
            this.queue.add( create() );
        }
    }
    public synchronized void add( T o ) {
        this.queue.add( o );
    }
    
    public synchronized T get() {
        if ( queue.isEmpty() ) {
            return create();
        } else {
            return queue.poll();
        }
    }
}
