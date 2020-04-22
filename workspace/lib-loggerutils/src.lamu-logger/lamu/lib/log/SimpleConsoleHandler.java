package lamu.lib.log;

import java.util.logging.ConsoleHandler;
import java.util.logging.Level;
import java.util.logging.LogRecord;

public class SimpleConsoleHandler extends ConsoleHandler {
	@Override
	public void publish(LogRecord record) {
		super.publish(record);
    	Logger.logSimpleConsole( Level.WARNING, record );
	}
}
