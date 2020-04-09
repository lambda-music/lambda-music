package lamu.lib.scheme;

import java.lang.invoke.MethodHandles;
import java.util.logging.Level;

import gnu.lists.Pair;
import gnu.mapping.Symbol;
import lamu.lib.log.Logger;

/**
 * This class was formerly called "Descriptive" class.
 * This class was renamed on (Thu, 09 Apr 2020 09:04:12 +0900).
 * @author ats
 */
public final class SchemeDocument {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
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
}
