package pulsar.lib;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;

public interface ThreadInitializer<T> extends Runnable {
    public static <T> ThreadInitializer<T> createThreadInitializer(CurrentObject<T> currentObject, T thisObject) {
        return new DefaultThreadInitializer<T>( currentObject, thisObject );
    }
    public static  ThreadInitializer createMultipleThreadInitializer( Collection<ThreadInitializer> initializers ) {
        return new MultipleThreadInitializer( initializers );
    }
    public static  ThreadInitializer createMultipleThreadInitializer( ThreadInitializer ... initializers ) {
        return new MultipleThreadInitializer( Arrays.asList( initializers ) );
    }
    final class DefaultThreadInitializer<T> implements ThreadInitializer<T> {
        CurrentObject<T> currentObject;
        T thisObject;
        DefaultThreadInitializer(CurrentObject<T> currentObject, T thisObject ) {
            super();
            this.currentObject = currentObject;
            this.thisObject = thisObject;
        }
        @Override
        public void run() {
            currentObject.set( thisObject );
        }
    }
    final class MultipleThreadInitializer implements ThreadInitializer {
        Collection<ThreadInitializer> initializers;
        MultipleThreadInitializer( Collection<ThreadInitializer> initializers ) {
            super();
            this.initializers = new ArrayList<>( initializers );
        }
        @Override
        public void run() {
            for ( ThreadInitializer i : initializers ) {
                try {
                    i.run();
                } catch ( Throwable t ) {
                    t.printStackTrace();
                }
            }
        }
    }
}