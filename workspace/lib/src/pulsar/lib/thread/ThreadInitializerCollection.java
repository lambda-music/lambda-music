package pulsar.lib.thread;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.logging.Level;

import pulsar.lib.CurrentObject;
import pulsar.lib.log.PulsarLogger;

public class ThreadInitializerCollection implements Runnable, ThreadInitializerOwner, ThreadInitializerContainer {
    static final PulsarLogger LOGGER = PulsarLogger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) {
        LOGGER.log(Level.SEVERE, msg, e);
    }
    static void logInfo(String msg) {
        LOGGER.log(Level.INFO, msg);
    }
    static void logWarn(String msg) {
        LOGGER.log(Level.WARNING, msg);
    }
    
    /////////////////////////////////////////////////////////////////////////////////////////
    //
    // ADDED (Tue, 14 Jan 2020 11:34:27 +0900)
    // NOTE : ThreadInitializerCollection itself is a thread initializer.
    // SEE_THIS_TAG
    // 
    /////////////////////////////////////////////////////////////////////////////////////////
    private static final CurrentObject<ThreadInitializerCollection> currentObject = new CurrentObject<>( ThreadInitializerCollection.class );
    private final ThreadInitializer<ThreadInitializerCollection> threadInitializer =
            ThreadInitializer.createMultipleThreadInitializer( "ThreadInitializerCollection", this, 
                ThreadInitializer.createThreadInitializer( "ThreadInitializerCollection", currentObject, this ) );
            
    @Override
    public ThreadInitializer<ThreadInitializerCollection> getThreadInitializer() {
        return threadInitializer;
    }
    public static ThreadInitializerCollection getCurrent() {
        return currentObject.get();
    }
    public static boolean isPresent() {
        return currentObject.isPresent();
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    
    Object owner;
    public final String id;
    public ThreadInitializerCollection( String id, Object owner ) {
        this.id = id;
        this.owner = owner;
    }
    @Override
    public Object getThreadInitializerOwner() {
        return owner;
    }
    private final Collection<Runnable> threadInitializerList = new ArrayList<>();

    
    public void addThreadInitializer( ThreadInitializer i ) {
        if ( i.getThreadInitializerOwner() == this.getThreadInitializerOwner() ) {
            // The container manages its thread initializer by itself; 
            // threrefore we should ignore it.
        } else {
            threadInitializerList.add(i);
        }
    }
    
    public void addThreadInitializer( Object o ) {
        logInfo( "addThreadInitializer:" + o );
        if ( o == null )  {
            
        } else if ( o instanceof ThreadInitializer ) {
            addThreadInitializer( (ThreadInitializer) o );
        } else if ( o instanceof Runnable ) {
            threadInitializerList.add((Runnable)o );
        } else if ( o instanceof ThreadInitializerContainer ) {
            ThreadInitializerContainer container = (ThreadInitializerContainer)o;
            addThreadInitializer( container.getThreadInitializer() );
        } else {
            logWarn( "unsupported type : " + o );
        }
    }
    public void addAllThreadInitializer( Collection<? extends Object> rs ) {
        for ( Object o : rs ) {
            addThreadInitializer( o );
        }
    }
    public void deleteThreadInitializer( Object r ) {
        threadInitializerList.remove( r );
    }
    public void deleteAllThreadInitializer( Collection<Object> rs ) {
        threadInitializerList.removeAll( rs );
    }
    public Collection<Runnable> getThreadInitializerList() {
        return Collections.unmodifiableCollection( threadInitializerList );
    }

    
    {
        /*
         *  ADDED (Tue, 14 Jan 2020 11:34:27 +0900)
         *  Register itself to the thread initializer collection.
         *  See the comment in the upper part of this source code.
         *  SEE_THIS_TAG 
         */
        addThreadInitializer( threadInitializer );
    }
    
    private static void runAll( Collection<Runnable> threadInitializers ) {
        for ( Runnable r : threadInitializers ) {
            try {
                r.run();
            } catch ( Throwable t ) {
                logError( "", t );
            }
        }
    }

    public void initialize() {
        runAll( getThreadInitializerList() );
    }
    @Override
    public void run() {
        initialize();
    }
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        for ( Runnable i : threadInitializerList ) {
            sb.append( "  " );
            sb.append( i.toString() );
            sb.append( "\n" );
        }
        
        // TODO Auto-generated method stub
        return "[Collection:"+id+"\n" + sb.toString() + "]";
    }
}
