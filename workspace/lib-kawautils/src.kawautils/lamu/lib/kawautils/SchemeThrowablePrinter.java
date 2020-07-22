package lamu.lib.kawautils;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.regex.Pattern;

public class SchemeThrowablePrinter {
	public static String throwableToSchemeList(Throwable thrown) {
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

	public static String throwableToString(Throwable e) {
		StringWriter sw = new StringWriter();
	    PrintWriter w = new PrintWriter( sw );
	    String str="(error)";
	    try {
	        e.printStackTrace( w );
	        w.flush();
	        sw.flush();
			str = doubleQuote( sw.toString() );
	    } finally {
	        try {
	            sw.close();
	        } catch (IOException e1) {
	            e1.printStackTrace();
	        }
	        w.close();
	    }
		return str;
	}

    static Pattern ESCAPE_DOUBLE_QUOTATIONS = Pattern.compile( "\"" );
    public static String escapeDoubleQuotation( String s ) {
        return ESCAPE_DOUBLE_QUOTATIONS.matcher(s).replaceAll( "\\\\\"" );
    }
	public static String doubleQuote( String s ) {
	    return "\"" + escapeDoubleQuotation(s) + "\"";
	}

}
