package lamu.lib.log;

import java.util.logging.Level;
import java.util.logging.LogRecord;

public class Logger {
    public static Logger getLogger( String name ) {
        return new Logger( name );
    }
    public static class State {
        boolean enabled = true;
        public boolean isEnabled() {
            return enabled;
        }
        public void setEnabled(boolean enabled) {
            this.enabled = enabled;
        }
    }
    public static class TemporaryDisable implements Runnable {
        private final Runnable runnable;
        public TemporaryDisable(Runnable runnable) {
            super();
            this.runnable = runnable;
        }
        @Override
        public void run() {
            try {
                getState().setEnabled(false);
                this.runnable.run();
            } finally {
                getState().setEnabled(true);
            }
        }
    }
    public static Runnable temporaryDisable( Runnable r ) {
        return new TemporaryDisable( r );
    }
    private static final ThreadLocal<Logger.State> threadLocal = new ThreadLocal<Logger.State>() {
        @Override
        protected Logger.State initialValue() {
            return new State();
        }
    };
    public static Logger.State getState() {
        return threadLocal.get();
    }
    public static void clearState() {
        threadLocal.remove();
    }

    private Level requiringLevel;
    private java.util.logging.Logger logger;
    public Logger( String name ) {
        this.logger = java.util.logging.Logger.getLogger( name );
        this.requiringLevel = Level.SEVERE;
    }
    public void log( Level level, String msg, Throwable e ) {
        if ( ! getState().isEnabled() ) return;
        this.logger.log( level, msg, e );
        logSimpleConsole( requiringLevel, level,  msg, e );
    }
    public void log( Level level, String msg ) {
        if ( ! getState().isEnabled() ) return;
        this.logger.log( level, msg );
        logSimpleConsole( requiringLevel, level,  msg );
    }

    ///////////////////////////

    public static void logSimpleConsole( Level requiringLevel, LogRecord record ) {
        logSimpleConsole( requiringLevel, record.getLevel(),  record.getMessage().trim() , record.getThrown() );
    }
    public static boolean compareLevel( Level requiringLevel, Level level ) {
        return requiringLevel.intValue() <= level.intValue();
    }
    public static void logSimpleConsole( Level requiringLevel, Level level, String message, Throwable thrown ) {
        if ( compareLevel( requiringLevel, level ) ) {
            if ( message!= null && "".equals( message ) ) {
                SimpleConsole.getConsole().addText( message );
            }
            if (thrown != null ) {
                SimpleConsole.getConsole().addText( thrown );
            }
        }	
    }
    public static void logSimpleConsole( Level leastLevel, Level level, String message ) {
        if ( message!= null && "".equals( message ) ) {
            SimpleConsole.getConsole().addText( message );
        }
    }
}
