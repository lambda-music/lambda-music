package lamu.lib.stream;

import java.io.IOException;
import java.io.InputStream;
import java.lang.invoke.MethodHandles;
import java.util.logging.Level;

import lamu.lib.app.ApplicationComponent;
import lamu.lib.log.Logger;

public class StreamPump implements ApplicationComponent, Runnable {
    protected static final Logger LOGGER = Logger.getLogger(MethodHandles.lookup().lookupClass().getName());
    protected static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    protected static void logInfo(String msg) { LOGGER.log(Level.INFO, msg); }
    protected static void logWarn(String msg) { LOGGER.log(Level.WARNING, msg); }
    protected static void logWarn(Throwable e) { LOGGER.log(Level.WARNING, "warning", e); }

    int bufSize = 1024*1024;
    InputStream in;
    transient Thread thread;
    public StreamPump( InputStream in ) {
        super();
        this.in = in;
    }

    ApplicationComponent parent;
    
    @Override
    public void setParentApplicationComponent(ApplicationComponent parent) {
        this.parent = parent;
    }
    
    @Override
    public ApplicationComponent getParentApplicationComponent() {
        return parent;
    }
    
    @Override
    public synchronized void processInit() {
        if ( thread != null )
            return;
        
        thread = new Thread( this , "pump[" + this.toString() + "]" );
        thread.setDaemon(true);
        thread.start();
    }
    
    @Override
    public synchronized void processQuit() {
        if ( thread == null )
            return;
        thread.interrupt();
    }
    
    @Override
    public void run() {
        byte[] buf = new byte[ bufSize ];
        try {
            for(;;) {
                int size;
                size = in.read( buf );
                if ( size < 0 ) {
                    break;
                }
            }
        } catch (IOException e) {
            LOGGER.log( Level.SEVERE, "an exception at a pump object occured", e );
        }
    }
    
}
