package quartz.lib.scheme.doc;

import java.lang.invoke.MethodHandles;
import java.util.logging.Level;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import gnu.lists.Pair;
import gnu.mapping.Procedure;
import gnu.mapping.SimpleSymbol;
import gnu.mapping.Symbol;
import quartz.lib.log.PulsarLogger;

public class Descriptive {
    static final PulsarLogger LOGGER = PulsarLogger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) {
        LOGGER.log(Level.SEVERE, msg, e);
    }
    static void logInfo(String msg) {
        LOGGER.log(Level.INFO, msg);
    }
    static void logWarn(String msg) {
        LOGGER.log(Level.WARNING, msg);
    }
//    private static final boolean DEBUG = false;

    //////////////////////////////////////////////////////////////////////////////////////////////////////
    //
    //////////////////////////////////////////////////////////////////////////////////////////////////////

    private static final String EXECUTE_SCHEME_DOCTAG_STRING = "**doc**".intern();
    private static final Symbol EXECUTE_SCHEME_DOCTAG = Symbol.valueOf( EXECUTE_SCHEME_DOCTAG_STRING );
    public static Object makeSchemeDocument( Object o ) {
        return Pair.make( EXECUTE_SCHEME_DOCTAG, o );
    }

    public static Object getSchemeDocument( Object o ) {
        if ( o instanceof Pair ) 
            return ((Pair)o).getCdr();
        else
            return false;
    }

    public static boolean isSchemeDocument(Object o ) {
        if ( o instanceof Pair ) 
            return EXECUTE_SCHEME_DOCTAG.equals( ((Pair)o).getCar() );
        else
            return false;
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////
    //
    //////////////////////////////////////////////////////////////////////////////////////////////////////

    public static <T> T procedureGet( Object proc, Object key, Object defaultValue ) {
        if ( proc instanceof Procedure ) {
            return (T)((Procedure)proc).getProperty( key, defaultValue );
        } else {
            logWarn( "procedureGet(): WARNING:" + proc + " is not procedure" );
            return null;
        }
    }
    public static void procedureSet( Object proc, SimpleSymbol key, Object value ) {
        if ( proc instanceof Procedure ) {
            ((Procedure)proc).setProperty( key, value );
        } else {
            logWarn( "procedureSet(): WARNING:" + proc + " is not procedure" );
        }
    }

    private static final SimpleSymbol DESCRIPTION  = Symbol.valueOf( "pulsar-description" );
    public static String getDescription( Object proc ) {
        return (String)procedureGet( proc, DESCRIPTION, null );
    }
    public static void setDescription( Object proc, String description ) {
        procedureSet( proc, DESCRIPTION, description );
    }
    
    public static final SimpleSymbol DESCRIPTION_BEAN  = Symbol.valueOf( "pulsar-description-bean" );
    public static DescriptiveBean getDescriptionBean( Object proc ) {
        return procedureGet( proc, DESCRIPTION_BEAN, null );
    }
    public static void setDescriptionBean( Object proc, DescriptiveBean bean ) {
        procedureSet( proc, DESCRIPTION_BEAN, bean );
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////
    //
    //////////////////////////////////////////////////////////////////////////////////////////////////////
    
    public static String formatKawapad( String message, int textWidth ) {
        message = wrapMultiLine( message, textWidth ).trim() + "\n";
        message =
                "(#|\n"+
                prefixMultiLine( message, "   " )+
                "|#  help about-intro )";
        return message;
    }
    
    private static final Pattern IS_INDENTED= Pattern.compile( "^\\s+" );
    protected static final String wrapMultiLine( String s, int width ) {
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
    
    protected static final String prefixMultiLine( String s, String prefix ) {
        StringBuilder sb = new StringBuilder();
        String[] a = s.split( "\n" );
        for ( int i=0; i<a.length; i++ ) {
             sb.append(prefix).append( a[i] ).append( "\n" );
        }
        return sb.toString();
    }
    
    protected static String wrap( String s, int width ) {
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
}
