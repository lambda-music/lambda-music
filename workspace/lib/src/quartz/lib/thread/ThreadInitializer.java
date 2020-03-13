package quartz.lib.thread;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.logging.Level;
import java.util.logging.Logger;

import quartz.lib.CurrentObject;
import quartz.lib.log.SimpleConsoleLogger;

public interface ThreadInitializer<T> extends Runnable, ThreadInitializerOwner {
    static final Logger LOGGER = SimpleConsoleLogger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) {
        LOGGER.log(Level.SEVERE, msg, e);
    }
    static void logInfo(String msg) {
        LOGGER.log(Level.INFO, msg);
    }
    static void logWarn(String msg) {
        LOGGER.log(Level.WARNING, msg);
    }

    public static <T> ThreadInitializer<T> createThreadInitializer( String id, CurrentObject<T> currentObject, T thisObject ) {
        if ( id.equals( "" ) ) throw new Error( "id should be specified." );
        Object owner = null;
        return new DefaultThreadInitializer<T>( id, owner, currentObject, thisObject );
    }
    public static  ThreadInitializer createMultipleThreadInitializer( String id, Object owner, Runnable ... initializers ) {
        if ( id.equals( "" ) ) throw new Error( "id should be specified." );
        return new MultipleThreadInitializer( id ,owner, Arrays.asList( initializers ) );
    }

//    /**
//     * If the thread initializer of an object should not be added automatically 
//     * by {@link ThreadInitializerCollection} , you should override this method to return true in order to
//     * prevend the {@link ThreadInitializerCollection} to add this thread initializer. 
//     * @return
//     */
//    default boolean isManagingIndependently() {
//        return false;
//    }

    final class DefaultThreadInitializer<T> implements ThreadInitializer<T> {
        String id;
        Object owner;
        CurrentObject<T> currentObject;
        T thisObject;
        DefaultThreadInitializer(String id, Object owner, CurrentObject<T> currentObject, T thisObject ) {
            super();
            this.id = id;
            this.currentObject = currentObject;
            this.thisObject = thisObject;
        }
        @Override
        public Object getThreadInitializerOwner() {
            return owner;
        }
        @Override
        public void run() {
            currentObject.set( thisObject );
        }
        @Override
        public String toString() {
            return "[Single:" + id + "]";
        }
    }
    final class MultipleThreadInitializer implements ThreadInitializer {
        String id;
        Object owner;
        Collection<Runnable> initializers;
        MultipleThreadInitializer( String id, Object owner, Collection<Runnable> initializers ) {
            super();
            this.initializers = new ArrayList<>( initializers );
            this.id = id;
        }
        @Override
        public Object getThreadInitializerOwner() {
            return owner;
        }
        @Override
        public void run() {
            for ( Runnable i : initializers ) {
                try {
                    if ( false )
                        logInfo( "Multiple(" + id + ") Executing : " + i.toString() );
                    i.run();
                } catch ( Throwable t ) {
                    t.printStackTrace();
                }
            }
        }
        @Override
        public String toString() {
            StringBuilder sb = new StringBuilder();
            for ( Runnable i : initializers ) {
                sb.append( i.toString() );
                sb.append( "," );
            }
            if ( 0 < sb.length() )
                sb.setLength( sb.length() -1 );
            return "[Multi:" + id + ":" + sb.toString() + "]";
        }
    }
}