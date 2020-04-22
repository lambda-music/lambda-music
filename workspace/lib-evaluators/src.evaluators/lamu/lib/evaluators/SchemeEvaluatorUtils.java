package lamu.lib.evaluators;

import java.io.File;
import java.lang.invoke.MethodHandles;
import java.util.logging.Level;

import lamu.lib.log.Logger;
import lamu.lib.log.SimpleConsole;

public class SchemeEvaluatorUtils {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    public static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }
    
    public static Object executeExternalFile( Runnable threadInitializer, String fileType, File scriptFile ) {
        // Read user's configuration file. If any problem is occurred, print its
        // stacktrace in the stderr, and then continue the process.
        try {
            logInfo( "Loading " + scriptFile.getName() );
            if ( scriptFile.exists() || scriptFile.isFile() ) {
                SchemeResult result = new SchemeEvaluator().evaluate( threadInitializer, scriptFile );
                result.throwIfError();
                return result.getValue(); 
            } else {
                logInfo( "The " + fileType + " file \"" + scriptFile.getPath() + "\" does not exist. Ignored." );
                return null;
            }
        } catch (Throwable e) {
        	SimpleConsole.getConsole().addText(e);
            logError( "Ignored an error : ", e );
            return null;
        }
    }

}
