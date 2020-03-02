package quartz.lib.scheme;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;

import gnu.kawa.io.OutPort;
import gnu.mapping.Procedure;

/*
 * moved from Kawapad.java (Mon, 29 Jul 2019 19:31:42 +0900)
 */
// TODO integrate all prettyPrint things.

public class SchemePrinter {
    public static String printSchemeValue( Object value ) {
        return schemeValuePrinter.format( value );
    }
    public static String printDocument( Object value ) {
        return documentPrinter.format( value );
    }

    public static interface Formatter {
        String format( Object value );
    }
    public static final Formatter DEFAULT_SCHEME_VALUE_PRINTER = new Formatter() {
        @Override
        public String format(Object value) {
            try {
                return SchemePrinter.prettyPrintProc( value );
            } catch (Throwable e) {
                e.printStackTrace();
                return "Internal Error : failed to print the object (" +  e.getMessage() + ")";
            }
        }
    };
    public static final Formatter DEFAULT_DOCUMENT_PRINTER = new Formatter() {
        @Override
        public String format(Object value) {
            try {
                return SchemePrinter.normalPrintProc( value );
            } catch (Throwable e) {
                e.printStackTrace();
                return "Internal Error : failed to print the object (" +  e.getMessage() + ")";
            }
        }
    };
    private static Formatter schemeValuePrinter   = DEFAULT_SCHEME_VALUE_PRINTER;
    private static Formatter documentPrinter      = DEFAULT_DOCUMENT_PRINTER;
    
    public static Formatter getSchemeValuePrinter() {
        return schemeValuePrinter;
    }
    public static void setSchemeValuePrinter(Formatter schemeValuePrinter) {
        SchemePrinter.schemeValuePrinter = schemeValuePrinter;
    }
    public static void setSchemeValuePrinter() {
        SchemePrinter.schemeValuePrinter = DEFAULT_SCHEME_VALUE_PRINTER;
    }
    public static Formatter getDocumentPrinter() {
        return documentPrinter;
    }
    public static void setDocumentPrinter(Formatter documentPrinter) {
        SchemePrinter.documentPrinter = documentPrinter;
    }
    public static void setDocumentPrinter() {
        SchemePrinter.documentPrinter = DEFAULT_DOCUMENT_PRINTER;
    }

    
    public static String normalPrintProc(Object resultObject)  {
        return SchemePrinter.printProc( kawa.lib.ports.display, resultObject );
    }
    public static String prettyPrintProc(Object resultObject) {
        return SchemePrinter.printProc( kawa.lib.ports.write, resultObject );
    }
    
    private static String printProc( Procedure print_proc, Object resultObject ) {
        StringWriter out = new StringWriter();
        try {
            OutPort outPort = new OutPort( out, false, true );
            SchemeUtils.toString( print_proc.apply2( resultObject, outPort ) );
            outPort.flush();
            return out.toString();
        } catch (Throwable e) {
            e.printStackTrace();
            return t2s( e );
        } finally {
            try {
                out.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }
    static String t2s(Throwable e) {
        StringWriter w = new StringWriter();
        PrintWriter p = new PrintWriter( w );
        e.printStackTrace( p );
        p.flush();
        String s = w.toString();
        return s;
    }
}
