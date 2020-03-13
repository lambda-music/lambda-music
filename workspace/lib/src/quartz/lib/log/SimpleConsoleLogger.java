package quartz.lib.log;

import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

/**
 * This class is a subclass of {@link Logger} class which picks up every
 * log-record which log-level exceeds the specified log-level to {@link SimpleConsole} .
 */
public class SimpleConsoleLogger extends Logger {
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
    	if ( Level.WARNING.intValue() <= record.getLevel().intValue() ) {
    		String msg = record.getMessage().trim();
			if ( msg!= null && "".equals( msg ) ) {
    			SimpleConsole.getConsole().addText( msg );
    		}
			Throwable thrown = record.getThrown();
    		if (thrown != null ) {
    			SimpleConsole.getConsole().addText( thrown );
    		}
    	}
    }
//    public void log(Level l, String msg, Throwable e) {
//        super.log( l, msg, e );
//        SimpleConsole.getConsole().addText( e );
//    }
}
