package lamu.lib.scheme.doc.old;

import java.lang.invoke.MethodHandles;
import java.util.logging.Level;

import gnu.mapping.Procedure;
import gnu.mapping.SimpleSymbol;
import gnu.mapping.Symbol;
import lamu.lib.log.Logger;

/**
 * (Thu, 09 Apr 2020 08:57:44 +0900)
 * Created.
 * 
 * (Thu, 09 Apr 2020 13:37:47 +0900)
 * LamuDocumentProcedure class was renamed from Descriptive Procedure.
 *  
 * @author ats
 */
public class DescriptiveProcedure {
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

    public static void procedureSet( Object proc, SimpleSymbol key, Object value ) {
        if ( proc instanceof Procedure ) {
            ((Procedure)proc).setProperty( key, value );
        } else {
            logWarn( "procedureSet(): WARNING:" + proc + " is not procedure" );
        }
    }
    public static <T> T procedureGet( Object proc, Object key, Object defaultValue ) {
        if ( proc instanceof Procedure ) {
            return (T)((Procedure)proc).getProperty( key, defaultValue );
        } else {
            logWarn( "procedureGet(): WARNING:" + proc + " is not procedure" );
            return null;
        }
    }

    private static final SimpleSymbol DESCRIPTION_BEAN  = Symbol.valueOf( "lamu-description-bean" );
    public static void setDescriptionBean( Object proc, DescriptiveBean bean ) {
        procedureSet( proc, DESCRIPTION_BEAN, bean );
    }

    public static DescriptiveBean getDescriptionBean( Object proc ) {
        return procedureGet( proc, DESCRIPTION_BEAN, null );
    }

    private static final SimpleSymbol DESCRIPTION  = Symbol.valueOf( "lamu-description" );
    public static void setDescription( Object proc, String description ) {
        procedureSet( proc, DESCRIPTION, description );
    }

    public static String getDescription( Object proc ) {
        return (String)procedureGet( proc, DESCRIPTION, null );
    }
}
