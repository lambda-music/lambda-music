package quartz.lib.scheme;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.lang.invoke.MethodHandles;
import java.util.logging.Level;

import quartz.lib.log.PulsarLogger;

public interface Evaluator {
    static final PulsarLogger LOGGER = PulsarLogger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }

    abstract SchemeResult evaluate(
            Runnable threadInitializer, 
            Reader schemeScript, 
            File currentDirectory, 
            File currentFile,
            String currentURI );

    abstract void reset();

    
    default SchemeResult evaluate( 
            Runnable threadInitializer, 
            String schemeScriptString, 
            File currentDirectory, 
            File currentFile, 
            String currentURI ) 
    {
        return evaluate( 
            threadInitializer, 
            new StringReader( schemeScriptString ),
            currentDirectory, 
            currentFile, 
            currentURI );
    }
    default SchemeResult evaluate( 
            Runnable threadInitializer, 
            String schemeScriptString, 
            String currentURI ) 
    {
        return evaluate( 
            threadInitializer,
            new StringReader( schemeScriptString ),
            null, 
            null, 
            currentURI );
    }
    default SchemeResult evaluate( 
            String schemeScriptString, 
            String currentURI ) 
    {
        return evaluate( 
            null,
            new StringReader( schemeScriptString ),
            null, 
            null, 
            currentURI );
    }

    default SchemeResult evaluate( Runnable threadInitializer, File schemeScriptFile ) {
        try {
            return evaluate( 
                threadInitializer,
                new InputStreamReader( new FileInputStream( schemeScriptFile ) ), 
                schemeScriptFile.getParentFile(), 
                schemeScriptFile, 
                schemeScriptFile.getPath() 
                    );
        } catch ( IOException e ) {
            return SchemeResult.createError( e );
        }
    }


    default SchemeResult evaluate( Runnable threadInitializer, Class parentClass, String resourcePath ) {
        try {
            return evaluate( 
                threadInitializer, 
                new InputStreamReader( parentClass.getResource( resourcePath ).openStream() ), 
                null, 
                null, 
                resourcePath 
                    );
        } catch ( IOException e ) {
            return SchemeResult.createError( e );
        }
    }
    default SchemeResult evaluate( Class parentClass, String resourcePath ) {
        try {
            return evaluate( 
                null, 
                new InputStreamReader( parentClass.getResource( resourcePath ).openStream() ), 
                null, 
                null, 
                resourcePath ); 
        } catch ( IOException e ) {
            return SchemeResult.createError( e );
        }
    }
    
//    default SchemeResult reset( Runnable threadInitializer ) {
//        String schemeScript = "(reset-scheme)";
//        return this.evaluate( 
//            threadInitializer,
//            new StringReader( schemeScript ),
//            null,
//            null,
//            "reset" );
//    }

}