package lamu.lib.threads;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;

import lamu.lib.logging.Logger;

public class LamuThreadLocalInitializer implements Runnable {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }

    final ThreadLocal<String> threadLocal = new ThreadLocal<String>();
    
    final Map<LamuThreadLocal<? extends Object>,Object> threadLocalMap = new HashMap<LamuThreadLocal<? extends Object>, Object>();
    final List<Runnable> initializers = new ArrayList<>();
    public LamuThreadLocalInitializer() {
        LamuThreadLocal.preserve(this.threadLocalMap);
    }
    public void addInitializer( Runnable r ) {
        this.initializers.add( r );
    }
    public void restore() {
        if ( threadLocal.get() == null ) {
            LamuThreadLocal.restore(threadLocalMap);
            for ( Runnable r : initializers ) {
                try {
                    r.run();
                } catch ( Throwable t ) {
                    logError("",t);
                }
            }
            threadLocal.set("DONE");
            logInfo( "LamuThreadLocalInitializer : DONE" );
        } else {
            // do nothing
        }
    }
    @Override
    public void run() {
        this.restore();
    }
}
