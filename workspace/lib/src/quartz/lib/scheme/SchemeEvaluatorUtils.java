package quartz.lib.scheme;

import java.io.File;
import java.io.InputStreamReader;
import java.io.Reader;
import java.lang.invoke.MethodHandles;
import java.util.logging.Level;

import gnu.expr.Language;
import gnu.kawa.io.InPort;
import gnu.kawa.io.Path;
import kawa.standard.Scheme;
import quartz.lib.log.SimpleConsoleLogger;

public class SchemeEvaluatorUtils {
    static final SimpleConsoleLogger LOGGER = SimpleConsoleLogger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }
    
    public static void executeExternalFile(Scheme scheme, Runnable threadInitializer, String fileType, File scriptFile) {
        // Read user's configuration file. If any problem is occurred, print its
        // stacktrace in the stderr, and then continue the process.
        try {
            logInfo( "Loading " + scriptFile.getName() );
            if ( scriptFile.exists() || scriptFile.isFile() ) {
                new SchemeEvaluator( scheme ).evaluate( threadInitializer, scriptFile ).throwIfError();
            } else {
                logInfo( "The " + fileType + " file \"" + scriptFile.getPath() + "\" does not exist. Ignored." );
            }
        } catch (Throwable e) {
            logError( "Ignored an error : ", e);
        }
    }
    
    // Execute script in the current context; that is, it is done without any initialization for threads/environmens. 
    public static void executeInTheCurrentContext( Class parentClass, String resourcePath ) {
        Scheme scheme = (Scheme) Language.getDefaultLanguage();
        try ( Reader schemeScript = new InputStreamReader( parentClass.getResource( resourcePath ).openStream() ); ) {
            scheme.eval( new InPort( schemeScript, Path.valueOf( resourcePath ) ) );
        } catch (Throwable e) {
            logError( "" , e );
        }
    }
}
