package pulsar.lib.scheme.scretary;

import java.lang.invoke.MethodHandles;
import java.util.Collection;
import java.util.logging.Level;
import java.util.logging.Logger;

public interface ShutdownHook {
	static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
	static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE,   msg, e   ); }
	static void logInfo (String msg             ) { LOGGER.log(Level.INFO,     msg      ); }
	static void logWarn (String msg             ) { LOGGER.log(Level.WARNING,  msg      ); }

	Collection<Runnable> getShutdownHookList();
    default void addShutdownHook( Runnable runnable ) {
    	synchronized ( this.getShutdownHookList() ) {
    		this.getShutdownHookList().add( runnable );
    	}
    }
    default void removeShutdownHook( Runnable runnable ) {
    	synchronized ( this.getShutdownHookList() ) {
    		this.getShutdownHookList().remove( runnable );
    	}
    }
    default void executeShutdownHook() {
    	synchronized ( this.getShutdownHookList() ) {
    		for ( Runnable r : this.getShutdownHookList() ) {
    			try {
    				r.run();
    			} catch ( Throwable e ) {
    				logError("", e);
    			}
    		}
    	}
    }
}

