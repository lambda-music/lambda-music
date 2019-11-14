package pulsar.lib.scheme;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;

import gnu.mapping.Values;

public final class SchemeResult {
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
    public void throwIfError() {
        if ( ! succeeded() ) {
            throw new RuntimeException( this.error );
        }
    }
    public boolean isEmpty() {
        return this.error == null && ( this.value == null || Values.empty.equals( this.value ) );
    }
}