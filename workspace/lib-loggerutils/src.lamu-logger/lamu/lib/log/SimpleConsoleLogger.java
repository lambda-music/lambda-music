package lamu.lib.log;

import java.util.logging.Level;
import java.util.logging.LogRecord;

/**
 * This class is a subclass of {@link Logger} class which picks up every
 * log-record which log-level exceeds the specified log-level to {@link SimpleConsole} .
 */
public class SimpleConsoleLogger extends java.util.logging.Logger {
	private final Level consoleLevel;
    public SimpleConsoleLogger(String name, String resourceBundleName, Level consoleLevel ) {
		super(name, resourceBundleName);
		this.consoleLevel = consoleLevel;
	}
    public Level getConsoleLevel() {
		return consoleLevel;
	}
	public static SimpleConsoleLogger getLogger( String name, String resourceBundleName ) {
        return new SimpleConsoleLogger( name, null, Level.WARNING );
    }
	public static SimpleConsoleLogger getLogger( String name, String resourceBundleName, Level consoleLevel ) {
        return new SimpleConsoleLogger( name, null, consoleLevel );
    }
	
    public void log(LogRecord record) {
    	super.log(record);
    	Logger.logSimpleConsole( Level.WARNING, record );
    }
//    public void log(Level l, String msg, Throwable e) {
//        super.log( l, msg, e );
//        SimpleConsole.getConsole().addText( e );
//    }
}
