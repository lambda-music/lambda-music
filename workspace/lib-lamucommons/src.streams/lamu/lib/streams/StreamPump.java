package lamu.lib.streams;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.invoke.MethodHandles;
import java.util.logging.Level;

import lamu.lib.apps.ApplicationComponent;
import lamu.lib.logging.Logger;

public class StreamPump implements ApplicationComponent, Runnable {
    protected static final Logger LOGGER = Logger.getLogger(MethodHandles.lookup().lookupClass().getName());
    protected static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    protected static void logInfo(String msg) { LOGGER.log(Level.INFO, msg); }
    protected static void logWarn(String msg) { LOGGER.log(Level.WARNING, msg); }
    protected static void logWarn(Throwable e) { LOGGER.log(Level.WARNING, "warning", e); }

    int bufSize = 1024*1024;
    InputStream in;
    OutputStream out;
    volatile Thread thread;
    public StreamPump( InputStream in, OutputStream out ) {
        super();
        this.in = in;
        this.out=out;
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
                int size = in.read( buf, 0 ,buf.length );
                if ( size < 0 ) {
                    break;
                }
                out.write( buf,0, size );
            }
        } catch (IOException e) {
            if ( e.getMessage().contains( "Stream closed") )
                LOGGER.log( Level.INFO,    "Stream closed:"+ e.getMessage() );
            else
                LOGGER.log( Level.WARNING, "an exception at a pump object occured", e );
        }
    }
    
}
