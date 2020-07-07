package lamu.lib.log;

import java.util.logging.Level;
import java.util.logging.LogRecord;

public class SimpleConsoleHandler extends java.util.logging.Handler {
	
	public static boolean compareLevel( Level requiringLevel, Level level ) {
	    return requiringLevel.intValue() <= level.intValue();
	}
	public static void log( Level requiringLevel, Level level, String message, Throwable thrown ) {
	    if ( compareLevel( requiringLevel, level ) ) {
	        if ( message!= null && "".equals( message ) ) {
	            SimpleConsole.getConsole().addText( message );
	        }
	        if (thrown != null ) {
	            SimpleConsole.getConsole().addText( thrown );
	        }
	    }	
	}
	public static void log( Level requiringLevel, LogRecord record ) {
	    log( requiringLevel, record.getLevel(),  record.getMessage().trim() , record.getThrown() );
	}
	public static void log( Level leastLevel, Level level, String message ) {
	    if ( message!= null && "".equals( message ) ) {
	        SimpleConsole.getConsole().addText( message );
	    }
	}

	//////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	public static SimpleConsoleHandler createDefault() {
		return new SimpleConsoleHandler( new LamuLogFormatter(), java.util.logging.Level.SEVERE );
	}
	public static SimpleConsoleHandler create( java.util.logging.Formatter formatter, java.util.logging.Level level ) {
		return new SimpleConsoleHandler( formatter, level );
	}
	public SimpleConsoleHandler( java.util.logging.Formatter formatter,  java.util.logging.Level level ) {
		setFormatter( formatter );
		setLevel( level );
	}
	
	@Override
	public void publish( LogRecord record ) {
	    if ( compareLevel( getLevel(), record.getLevel() ) ) {
	    	SimpleConsole.getConsole().addText( getFormatter().format( record ) );
	    }
	}

	@Override
	public void flush() {
	}

	@Override
	public void close() throws SecurityException {
	}
}
