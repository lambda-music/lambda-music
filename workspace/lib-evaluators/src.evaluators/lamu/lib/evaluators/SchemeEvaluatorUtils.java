package lamu.lib.evaluators;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.lang.invoke.MethodHandles;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.concurrent.CountDownLatch;
import java.util.logging.Level;

import lamu.lib.kawautils.SchemeValues;
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
    
    public static File useResolve( File f ) {
        File file = SchemeEvaluatorImplementation.currentBaseFile.get();
        if ( file == null ) {
            throw new IllegalStateException( 
                "NO BAE FILE DEFINED ERROR : " +
                "Currently no base file is defined. Please save the current editting file, before loading files." );
        }
        
        File resolvedFile = new File( file.getParentFile(), f.getPath() );
        return resolvedFile;
    }
    
    static final class EvaluatorReceiverImplementation implements EvaluatorReceiver {
        final CountDownLatch latch = new CountDownLatch(2);
        volatile SchemeResult schemeResult;
        @Override
        public void receive(SchemeResult schemeResult) {
            this.schemeResult = schemeResult;
            this.latch.countDown();
        }
    }

    public static Object use( File f ) throws IOException {
        Evaluator evaluator = Evaluator.getCurrent();
        if ( evaluator == null ) {
            throw new IllegalStateException("NO EVALUATOR ERROR : currently, no evaluator was configured.");
        }
        
        ThreadManager threadManager = ThreadManager.getCurrent();
        if ( threadManager == null ) {
            throw new IllegalStateException("NO THREAD MANAGER ERROR : currently, no thread manager was configured.");
        }

        EvaluatorReceiverImplementation resultReceiver = new EvaluatorReceiverImplementation();
        File resolvedFile = useResolve(f);

        AsyncEvaluator.executeAsync(
            threadManager, 
            null, 
            new FileReader(resolvedFile),
            evaluator, 
            resultReceiver, 
            resolvedFile, 
            resolvedFile.getAbsolutePath() );
        
        resultReceiver.latch.countDown();
        try {
            resultReceiver.latch.await();
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
        return resultReceiver.schemeResult.getValue();
    }
    public static Object useRead( File file ) throws IOException {
        File resolvedFile = useResolve(file);
        return 
            SchemeValues.string2lisp(
            new String(
                Files.readAllBytes(
                    Paths.get( resolvedFile.toURI())),
                StandardCharsets.UTF_8));
    }

}
