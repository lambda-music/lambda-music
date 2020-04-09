package lamu.lib.doc;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * (Thu, 09 Apr 2020 09:02:08 +0900)
 * 
 * @author ats
 *
 */
public class LamuDocumentFormatterUtil {
    public static String wrap( String s, int width ) {
        Matcher m = Pattern.compile( "\\s+" ).matcher(s);
        StringBuilder sb = new StringBuilder();
        int head=0;
        int last=head;
        while ( m.find() ) {
            int curr = m.start();
            String stringToAdd = s.substring(last,curr);
            if ( ( curr-head ) < width ) {
                sb.append(' ');
                sb.append( stringToAdd );
                last = curr + m.group().length();
            } else {
                sb.append('\n');
                sb.append( stringToAdd );
                last = curr + m.group().length();
                head = last;
            }
        }
        {
            String stringToAdd = s.substring(last,s.length());
            sb.append(' ');
            sb.append( stringToAdd );
            sb.append('\n');
        }
        return sb.toString();
    }

    public static final String prefixMultiLine( String s, String prefix ) {
        StringBuilder sb = new StringBuilder();
        String[] a = s.split( "\n" );
        for ( int i=0; i<a.length; i++ ) {
             sb.append(prefix).append( a[i] ).append( "\n" );
        }
        return sb.toString();
    }

    public static final Pattern IS_INDENTED= Pattern.compile( "^\\s+" );
    public static final String wrapMultiLine( String s, int width ) {
            StringBuilder sb = new StringBuilder();
    //      String[] a = s.split("\\r\\n\\r\\n|\\n\\r\\n\\r|\\n\\n|\\r\\r" );
            String[] a = s.split( "\n\n" );
            for ( int i=0; i<a.length; i++ ) {
                // if the line starts with blank characters (that is, the line is indented),
                // then leave it as it is; otherwise, wrap it with the specified length.
                // (Thu, 29 Aug 2019 05:58:14 +0900)
                if ( IS_INDENTED.matcher( a[i] ).find()) {
                    sb.append(      a[i]               ).append( "\n\n" );
                } else {
    //              System.err.println( "SDDDD" );
                    sb.append( wrap(a[i],width).trim() ).append( "\n\n" );
                }
            }
            return sb.toString();
        }


    public static String formatKawapad( String message, int textWidth ) {
        message = wrapMultiLine( message, textWidth ).trim() + "\n";
        message =
                "(#|\n"+
                prefixMultiLine( message, "   " )+
                "|#  help about-intro )";
        return message;
    }

}
