package pulsar.lib;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.logging.Level;
import java.util.logging.Logger;

public class ThreadInitializerCollection implements Runnable {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) {
        LOGGER.log(Level.SEVERE, msg, e);
    }
    static void logInfo(String msg) {
        LOGGER.log(Level.INFO, msg);
    }
    static void logWarn(String msg) {
        LOGGER.log(Level.WARNING, msg);
    }

    private final Collection<Runnable> threadInitializerList = new ArrayList<>();
    public void addThreadInitializer( Runnable r ) {
        threadInitializerList.add( r );
    }
    public void addAllThreadInitializer( Collection<Runnable> rs ) {
        threadInitializerList.addAll( rs );
    }
    public void addAllThreadInitializerContainer( Collection<Object> rs ) {
        for ( Object o : rs ) {
            if ( o instanceof ThreadInitializerContainer ) {
                ThreadInitializer init = ((ThreadInitializerContainer)o).getThreadInitializer();
                if ( init != null ) {
                    threadInitializerList.add( init );
                }
            }
        }
    }
    public void deleteThreadInitializer( Runnable r ) {
        threadInitializerList.remove( r );
    }
    public void deleteAllThreadInitializer( Collection<Runnable> rs ) {
        threadInitializerList.removeAll( rs );
    }
    public Collection<Runnable> getThreadInitializerList() {
        return Collections.unmodifiableCollection( threadInitializerList );
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
}
