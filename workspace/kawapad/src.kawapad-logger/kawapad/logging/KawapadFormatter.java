package kawapad.logging;

import java.sql.Date;
import java.text.SimpleDateFormat;
import java.util.logging.LogRecord;

public class KawapadFormatter extends java.util.logging.Formatter {
	private final SimpleDateFormat formatter = new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss Z");
	@Override
	public synchronized String format(LogRecord record) {
		return String.format(
				"(define log-%04d '((no . %d)(logger . \"%s\" )(level . %s)(thread . \"%s\" )(time . \"(%s)\")(message . %s)%s))\n",
				record.getSequenceNumber(),
				record.getSequenceNumber(),
				record.getLoggerName(),
				record.getLevel(),
				record.getThreadID(),
				formatter.format( new Date( record.getMillis() )),
				formatMessage(record.getMessage()),
				formatThrown( record.getThrown() )
				);
	}
	private static String formatMessage(String message ) {
		String str = message.trim();
		if ( str.startsWith("(") && str.endsWith(")" ) )
			return str;
		else if ( str.startsWith("\"") && str.endsWith("\"" ) )
			return str;
		else
			return "\"" + str + "\"";
	}
	private static Object formatThrown(Throwable thrown) {
		if ( thrown == null)
			return "";
		StringBuilder sb = new StringBuilder();
		String INDENT = "    ";
		sb.append("\n");
		sb.append("  (throwable  ");
		sb.append("\""). append( thrown.getMessage() ).append( "\"\n" );
		for( StackTraceElement e : thrown.getStackTrace() ) {
			sb.append(INDENT).append("(" ).append(
						e.getClassName() + 
						"." + 
						e.getMethodName() + 
						" . " + 
						"\"" + 
						e.getFileName() + 
						":" + 
						e.getLineNumber() +  
						"\"");   
			sb.append(")\n");
		}
		sb.append("  )");
		return sb.toString();
	}
}
