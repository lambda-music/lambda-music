package lamu;

import java.util.Collection;
import java.util.regex.Pattern;

import pulsar.NoteListParser;
import quartz.lib.scheme.SchemePrinter;

public class LamuPrinter {
    static final Pattern REMOVE_LINE_BREAKS = Pattern.compile( "(\n\r|\r\n|\r|\n)", Pattern.MULTILINE );
    public static void init() {
        SchemePrinter.setSchemeValuePrinter( new SchemePrinter.Formatter() {
            @Override
            public String format(Object value) {
                if ( NoteListParser.isNotationList( value ) ) {
//                    gnu.kawa.io.PrettyWriter.lineLengthLoc.set( 1000 );
                    StringBuilder sb = new StringBuilder();
                    sb.append( "(" );
                    for ( Object o : ((Collection)value) ) {
                        sb.append( REMOVE_LINE_BREAKS.matcher( SchemePrinter.prettyPrintProc( o ) ).replaceAll( "" ) );
                        sb.append( "\n" );
                    }
                    if ( 0 < sb.length() ) 
                        sb.setLength( sb.length() -1 );
                    sb.append( ")");
                    return sb.toString();
                } else  {
//                    gnu.kawa.io.PrettyWriter.lineLengthLoc.set( 1000 );
                    return SchemePrinter.prettyPrintProc( value );
//                    return SchemePrinter.DEFAULT_SCHEME_VALUE_PRINTER.format( value );
                }
            }
        } );
    }
}
