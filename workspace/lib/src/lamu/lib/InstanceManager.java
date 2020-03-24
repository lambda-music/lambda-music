package lamu.lib;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;
import java.util.logging.Level;

import lamu.lib.log.PlainLogger;

/**
 * Close all of the processes when the JVM shuts down. 
 */
public class InstanceManager<T> {
    public static interface Processor<T> {
        void quit( T o ); 
        void kill( T o ); 
        boolean isAlive( T o ); 
        String getName( T o );
    }
    public static <T> InstanceManager<T> create(Processor<T> processor) {
        return new InstanceManager<T>(processor);
    }
    
    public static void shutdown() {
        synchronized ( ALL ) {
            logInfo( "InstanceManager shutdown-all request" );
            for ( InstanceManager<?> i : ALL ) {
                i.destroyAll();
            }
        }
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    
    static final int CLEANUP_FREQUENCY = 60*1000;
    
    static final PlainLogger LOGGER = PlainLogger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE,   msg, e   ); }
    static void logInfo (String msg             ) { LOGGER.log(Level.INFO,     msg      ); }
    static void logWarn (String msg             ) { LOGGER.log(Level.WARNING,  msg      ); }
    static final Timer TIMER = new Timer(true);
    
    static final List<InstanceManager> ALL = new ArrayList<>();
    
    private final InstanceManager.Processor<T> processor;
    InstanceManager(InstanceManager.Processor<T> processor) {
        super();
        this.processor = processor;
        Runtime.getRuntime().addShutdownHook( new Thread() {
            @Override
            public void run() {
                logInfo( "InstanceManager.shutdownHook" );
                destroyAll();
            }
        });
        
        synchronized ( TIMER ) {
            TIMER.schedule( new TimerTask() {
                @Override
                public void run() {
                    logInfo( "InstanceManager.cleanup" );
                    cleanupAll();
                }
            }, CLEANUP_FREQUENCY, CLEANUP_FREQUENCY + (int)( Math.random() * 1000 ) );
        }
        synchronized (ALL) {
            ALL.add( this );
        }
    }
    
    void report() {
        logInfo( "InstanceManager.left count=" + getInstances().size() );
    }
    
    boolean isAlive(T o) {
        return processor.isAlive(o);
    }
    void quitAndKill( T o ) {
        Runnable r = new Runnable() {
            @Override
            public void run() {
                processor.quit( o );
                logInfo( "InstanceManager.quit " + getName(o) );
                try {
                    Thread.sleep(1000);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
                if ( processor.isAlive(o)) {
                    processor.kill( o );
                    logInfo( "InstanceManager.killed " + getName(o) );
                }
            }
        };
        Thread t = new Thread( r );
        t.start();
    }
    String getName(T o) {
        return processor.getName( o );
    }

    private final List<T> instances = new ArrayList<>();
    private List<T> getInstances() {
        return instances;
    }
    public void register( T o ) {
        synchronized ( getInstances() ) {
            getInstances().add(o);
            logInfo( "InstanceManager.registered " + getName(o) );
            report();
        }
    }
    transient boolean destroyed =false;
    public void destroyAll() {
        synchronized ( getInstances() ) {
            if ( destroyed )
                return ;
            destroyed = true;
            
            logInfo( "==== destroyInstances() target count=" + getInstances().size() );
            for ( Iterator<T> i= getInstances().iterator(); i.hasNext(); ) {
                T o = i.next();
                quitAndKill( o );
            }
            report();
        }
    }
    public void cleanupAll() {
        synchronized ( getInstances() ) {
            for ( Iterator<T> i=getInstances().iterator(); i.hasNext(); ) {
                T o = i.next();
                if ( ! isAlive(o) ) {
                    i.remove();
                    logInfo( "InstanceManager.removed " + getName(o) );
                }
            }
            report();
        }
    }    
}