package pulsar.lib.scheme;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.invoke.MethodHandles;
import java.util.logging.Level;
import java.util.logging.Logger;

import gnu.mapping.Values;

public final class SchemeResult {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE,   msg, e   ); }
    static void logInfo (String msg             ) { LOGGER.log(Level.INFO,     msg      ); }
    static void logWarn (String msg             ) { LOGGER.log(Level.WARNING,  msg      ); }

    public static SchemeResult create(boolean isDocument, Object value, String valueAsString, Throwable error) {
        return new SchemeResult( isDocument, value, valueAsString, error );
    }
    public static SchemeResult createNull() {
        return SchemeResult.create( false, null, "#!null", null );
    }
    public static SchemeResult createSucceeded(boolean isDocument, Object value, String valueAsString ) {
        return new SchemeResult( isDocument, value, valueAsString, null );
    }
    public static SchemeResult createError( Throwable e ) {
        StringWriter sw = new StringWriter();
        PrintWriter w = new PrintWriter( sw );
        try {
            e.printStackTrace( w );
            w.flush();
            sw.flush();
            return SchemeResult.create( false, null, sw.toString(), e );
        } finally {
            try {
                sw.close();
            } catch (IOException e1) {
                e1.printStackTrace();
            }
            w.close();
        }
    }

    public static final Object UNKNOWN_CONTENT = new Object();
    
    public final boolean isDocument;
    public final Object value;
    public final String valueAsString;
    public final Throwable error;
    public final boolean succeeded() {
        return error == null;
    }
    private SchemeResult( boolean isDocument, Object value, String valueAsString, Throwable error ) {
        super();
        this.isDocument = isDocument;
        this.value = value;
        this.valueAsString = valueAsString;
        this.error = error;
    }
    public SchemeResult throwIfError() {
        if ( ! succeeded() ) {
            throw new RuntimeException( this.error );
        }
        return this;
    }
    public SchemeResult warnIfError() {
        if ( ! succeeded() ) {
            logError( "Error", this.error );
        }
        return this;
    }
    
    public boolean isEmpty() {
        return this.error == null && ( this.value == null || Values.empty.equals( this.value ) );
    }
    
    //////////////////////////////////////////////////////////////////////////////////////////////////////
    
    public static String formatResult( String s ) {
        if ( s == null )
            return null;
        else if ( s.equals( "" ) )
            return "";
        else
            return "#|\n" + endWithLineFeed( s ) + "|#\n";
    }
    
    public static String endWithLineFeed(String s) {
        if ( s == null )
            return null;
        else if ( s.equals( "" ) )
            return "";
        else if ( s.endsWith("\n" ) )
            return s;
        else
            return s + "\n"; 
    }
}