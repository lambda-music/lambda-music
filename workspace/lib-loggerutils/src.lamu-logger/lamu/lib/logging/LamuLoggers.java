package lamu.lib.logging;

import java.lang.invoke.MethodHandles;
import java.util.Arrays;
import java.util.List;
import java.util.logging.Handler;
import java.util.logging.Level;

import javax.swing.SwingUtilities;

public class LamuLoggers {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE,   msg, e   ); }
    static void logInfo (String msg             ) { LOGGER.log(Level.INFO,     msg      ); }
    static void logWarn (String msg             ) { LOGGER.log(Level.WARNING,  msg      ); }

	/**
	 *  The root logger's first handler is the default ConsoleHandler.
	 * @return
	 * the default ConsoleHandler
	 */
	public static List<java.util.logging.Handler> getDefaultConsoleHandler() {
	    java.util.logging.Logger rootLogger = LamuLoggers.getRootLogger();
	    return Arrays.asList( rootLogger.getHandlers() );
	}

	/**
	 * All the loggers inherit configuration from the root logger.  See:
	 * https://docs.oracle.com/javase/8/docs/technotes/guides/logging/overview.html#a1.3
	 * @return
	 * the root logger
	 */
	public static java.util.logging.Logger getRootLogger() {
	    java.util.logging.Logger rootLogger = java.util.logging.Logger.getLogger("");
		return rootLogger;
	}

	public static void installOnDefaultHandler() {
		getRootLogger().setLevel( java.util.logging.Level.ALL );
		Handler consoleHandler = getDefaultConsoleHandler().get(0);
		consoleHandler.setFormatter( LamuLogFormatter.create() );
	    SimpleConsoleHandler simpleConsoleHandler = SimpleConsoleHandler.createDefault();
		getRootLogger().addHandler( simpleConsoleHandler );
		consoleHandler.setLevel( java.util.logging.Level.INFO );
		simpleConsoleHandler.setLevel( java.util.logging.Level.SEVERE );
	}

    public static void main(final String... args) {
        installOnDefaultHandler();
        logInfo("Hello from the main thread");
        SwingUtilities.invokeLater(() -> logInfo("Hello from the event dispatch thread"));
        SwingUtilities.invokeLater(() -> LOGGER.log( java.util.logging.Level.SEVERE, " hello severe") );
        SwingUtilities.invokeLater(() -> LOGGER.log( java.util.logging.Level.INFO,   " hello info"  ) );
    }
}
