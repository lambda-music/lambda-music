package lamu.lib.kawautils;

import java.io.InputStreamReader;
import java.io.Reader;
import java.lang.invoke.MethodHandles;
import java.util.logging.Level;

import gnu.expr.Language;
import gnu.kawa.io.InPort;
import gnu.kawa.io.Path;
import gnu.mapping.Environment;
import kawa.standard.Scheme;
import lamu.lib.log.Logger;

public class SchemeEvaluation {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }

    // Execute script in the current context; that is, it is done without any initialization for threads/environmens. 
    public static void executeResourceInTheCurrentContext( Class parentClass, String resourcePath ) {
        Scheme scheme = (Scheme) Language.getDefaultLanguage();
        if ( scheme == null ) {
            throw new IllegalStateException( "missing the current default language" );
        }
        try ( Reader schemeScript = new InputStreamReader( parentClass.getResource( resourcePath ).openStream() ); ) {
            scheme.eval( new InPort( schemeScript, Path.valueOf( resourcePath ) ) );
        } catch (Throwable e) {
            logError( "" , e );
        }
    }

    public static void executeInTheCurrentContext( String script ) {
        Scheme scheme = (Scheme) Language.getDefaultLanguage();
        Environment.setCurrent( scheme.getEnvironment() );
        try {
            scheme.eval(script);
        } catch (Throwable e) {
            logError( "" , e );
        }
    }

}
