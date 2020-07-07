package kawapad.logging;

import java.util.logging.Handler;

import lamu.lib.log.LamuLogFormatter;
import lamu.lib.log.LamuLoggers;

public class KawapadLoggers {
	public static void installOnDefaultHandler() {
		LamuLoggers.getRootLogger().setLevel( java.util.logging.Level.INFO );
		Handler consoleHandler = LamuLoggers.getDefaultConsoleHandler().get(0);
		consoleHandler.setFormatter( LamuLogFormatter.create() );
		consoleHandler.setLevel( java.util.logging.Level.INFO );
		
//	    SimpleConsoleHandler simpleConsoleHandler = SimpleConsoleHandler.createDefault();
//	    simpleConsoleHandler.setLevel( java.util.logging.Level.SEVERE );
//	    LamuLoggers.getRootLogger().addHandler( simpleConsoleHandler );
		KawapadHandler kawapadHandler = new KawapadHandler();
		kawapadHandler.setFormatter( LamuLogFormatter.create() );
		kawapadHandler.setLevel( java.util.logging.Level.SEVERE );
		LamuLoggers.getRootLogger().addHandler( kawapadHandler );
	}
}
