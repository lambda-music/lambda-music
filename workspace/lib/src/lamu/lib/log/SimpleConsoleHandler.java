package lamu.lib.log;

import java.util.logging.ConsoleHandler;
import java.util.logging.Level;
import java.util.logging.LogRecord;

public class SimpleConsoleHandler extends ConsoleHandler {
	@Override
	public void publish(LogRecord record) {
		super.publish(record);
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
}
