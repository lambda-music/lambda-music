package metro;

import java.util.ArrayDeque;

public abstract class Pool<T> {
    private final ArrayDeque<T> queue = new ArrayDeque<T>();
    protected Pool( int initialSize ) {
        for ( int i=0; i<initialSize; i++ ) {
            this.queue.add( create() );
        }
    }
    protected abstract T create();
    protected T initializeValue( T o ) {
    	return o;
    }
    public synchronized T withdraw() {
        if ( queue.isEmpty() ) {
            return initializeValue( create() );
        } else {
            return initializeValue( queue.poll() );
        }
    }
    protected T finalizeValue( T o ) {
    	return o;
    }
    public synchronized void deposit( T o ) {
        this.queue.add( finalizeValue(o) );
    }
}
