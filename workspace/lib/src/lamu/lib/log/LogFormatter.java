package lamu.lib.log;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.invoke.MethodHandles;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Optional;
import java.util.logging.Formatter;
import java.util.logging.Level;
import java.util.logging.LogRecord;

import javax.swing.SwingUtilities;

public class LogFormatter extends Formatter {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE,   msg, e   ); }
    static void logInfo (String msg             ) { LOGGER.log(Level.INFO,     msg      ); }
    static void logWarn (String msg             ) { LOGGER.log(Level.WARNING,  msg      ); }
    
    public LogFormatter() {
    }
    
    static Optional<Thread> getThread(long threadId) {
        return Thread.getAllStackTraces().keySet().stream()
                .filter(t -> t.getId() == threadId)
                .findFirst();
    }
    static int MAX_CLASS_NAME = 8;
    //                      private final String format = LoggingSupport.getSimpleFormat();
    private final String format = "%1$tY-%1$tm-%1$td %1$tH:%1$tM:%1$tS %4$4s:%7$-20s %3$-"+MAX_CLASS_NAME+"s || %5$s%6$s%n";
    private final Date dat = new Date();
    public synchronized String format(LogRecord record) {
        dat.setTime(record.getMillis());
        String source;
        if (record.getSourceClassName() != null) {
            source = record.getSourceClassName();
            if (record.getSourceMethodName() != null) {
                source += " " + record.getSourceMethodName();
            }
        } else {
            source = record.getLoggerName();
        }
        String message = formatMessage(record);
        String throwable = "";
        if (record.getThrown() != null) {
            StringWriter sw = new StringWriter();
            PrintWriter pw = new PrintWriter(sw);
            pw.println();
            record.getThrown().printStackTrace(pw);
            pw.close();
            throwable = sw.toString();
        }
        
        int threadId = record.getThreadID();
        String threadName = getThread(threadId)
                .map(Thread::getName)
                .orElseGet(() -> "Thread with ID " + threadId);
        
        return String.format(format,
            dat,
            limitString( source, 20 ),
            limitString( shortenClassName( record.getLoggerName() ), MAX_CLASS_NAME) ,
            record.getLevel().getLocalizedName().substring(0,4),
            message,
            throwable,
            limitString( threadName, 20 )
                );
    }
    private String shortenClassName(String s) {
        int i = s.lastIndexOf( '.' );
        if ( 0<=i )
            return s.substring(i+1);
        else
            return s;
    }
    private String limitString(String s, int length ) {
        if ( s.length() < length ) 
            return s;
        else
            return s.substring(0,length);
    }
    
    private static Formatter getMinimalFormatter() {
        return new LogFormatter();
    }
    public static void main(final String... args) {
        init();
        logInfo("Hello from the main thread");
        SwingUtilities.invokeLater(() -> logInfo("Hello from the event dispatch thread"));
    }
    public static void init() {
        getDefaultConsoleHandler().get(0).setFormatter( getMinimalFormatter() );
    }
    
    static List<java.util.logging.Handler> getDefaultConsoleHandler() {
        // All the loggers inherit configuration from the root logger. See:
        // https://docs.oracle.com/javase/8/docs/technotes/guides/logging/overview.html#a1.3
        java.util.logging.Logger rootLogger = java.util.logging.Logger.getLogger("");
        // The root logger's first handler is the default ConsoleHandler
        return Arrays.asList( rootLogger.getHandlers() );
    }
}