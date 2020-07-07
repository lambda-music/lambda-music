package lamu.lib.logging;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.invoke.MethodHandles;
import java.util.Date;
import java.util.Optional;
import java.util.logging.Formatter;
import java.util.logging.Level;
import java.util.logging.LogRecord;

public class LamuLogFormatter extends Formatter {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE,   msg, e   ); }
    static void logInfo (String msg             ) { LOGGER.log(Level.INFO,     msg      ); }
    static void logWarn (String msg             ) { LOGGER.log(Level.WARNING,  msg      ); }
    
    public LamuLogFormatter() {
    }
    
    static Optional<Thread> getThread(long threadId) {
        return Thread.getAllStackTraces().keySet().stream()
                .filter(t -> t.getId() == threadId)
                .findFirst();
    }
    static int MAX_CLASS_NAME = 32;
    //                      private final String format = LoggingSupport.getSimpleFormat();
    private final String format = "%1$tY-%1$tm-%1$td %1$tH:%1$tM:%1$tS[THREAD:%7$-20s|CLASS:%3$-"+MAX_CLASS_NAME+"s] [%4$4s] %5$s%6$s%n";
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
    
    public static Formatter create() {
        return new LamuLogFormatter();
    }
}