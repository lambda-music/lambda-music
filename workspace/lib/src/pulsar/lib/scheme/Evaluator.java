package pulsar.lib.scheme;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;

public interface Evaluator {
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