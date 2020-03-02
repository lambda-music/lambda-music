package quartz.lib.app;

import java.lang.invoke.MethodHandles;
import java.util.logging.Level;

import quartz.lib.log.SimpleConsoleLogger;

public interface ApplicationComponent {
    static final SimpleConsoleLogger LOGGER = SimpleConsoleLogger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e)  { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)                { LOGGER.log(Level.INFO, msg); }
    static void logWarn(String msg)                { LOGGER.log(Level.WARNING, msg); }

    void setParentApplicationComponent( ApplicationComponent parent );
    ApplicationComponent getParentApplicationComponent();
    
    public default void requestInit() {
        ApplicationComponent parent = getParentApplicationComponent();
        if ( parent != null && (parent instanceof ApplicationVessel)) {
            try {
                ((ApplicationVessel)parent).requestInit();
            } catch ( Throwable t ) {
                logError( "", t );
            }
        } else {
            if ( this instanceof ApplicationVessel ) {
                ((ApplicationVessel)this).getThreadInitializerCollection().initialize();
            }
            this.processInit();
        }
    }
    public default void requestQuit() {
        ApplicationComponent parent = getParentApplicationComponent();
        if ( parent != null && (parent instanceof ApplicationVessel)) {
            try {
                ((ApplicationVessel)parent).requestQuit();
            } catch ( Throwable t ) {
                logError( "", t );
            }
        } else {
            this.processQuit();
        }
    }

    void processInit();
    void processQuit();
}
