package lamu.lib.logging;

import java.util.logging.LogRecord;

public class Logger extends java.util.logging.Logger {
    public static Logger getLogger( String name ) {
        Logger logger = new Logger( name, null );
        logger.setParent( LamuLoggers.getRootLogger() );
		return logger;
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
    public Logger(String name, String resourceBundleName) {
    	super(name, resourceBundleName);
    }
    @Override
    public void log(LogRecord record) {
        if ( ! getState().isEnabled() ) return;
    	super.log(record);
    }
}
