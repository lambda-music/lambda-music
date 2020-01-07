package pulsar.lib.scheme;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.lang.invoke.MethodHandles;
import java.util.logging.Level;
import java.util.logging.Logger;

public interface Evaluator {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }

    abstract SchemeResult evaluate(
            Runnable threadInitializer, 
            Reader schemeScript, 
            File currentDirectory, 
            File currentFile,
            String currentURI );

    
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

    default SchemeResult evaluate( Runnable threadInitializer, File schemeScriptFile ) throws IOException {
        return evaluate( 
            threadInitializer,
            new InputStreamReader( new FileInputStream( schemeScriptFile ) ), 
            schemeScriptFile.getParentFile(), 
            schemeScriptFile, 
            schemeScriptFile.getPath() 
            );
    }


    default SchemeResult evaluate( Runnable threadInitializer, Class parentClass, String resourcePath ) throws IOException {
        return evaluate( 
            threadInitializer, 
            new InputStreamReader( parentClass.getResource( resourcePath ).openStream() ), 
            null, 
            null, 
            resourcePath 
            );
    }
    default SchemeResult evaluate( Class parentClass, String resourcePath ) throws IOException {
        return evaluate( null, parentClass, resourcePath ); 
    }

}