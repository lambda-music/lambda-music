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

    public static String USE_MAINFILE = "main.scm";

    /**
     * 
     * @param threadInitializer
     *     Specify a thread initializer; null if no initializer is to be specified.
     * @param fileType
     * @param scriptFile
     * 
     * @return
     */
    public static Object executeStatic( Runnable threadInitializer, String fileType, File scriptFile ) {
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

    /**
     * @param threadInitializer
     *     Specify a thread initializer; null if no initializer is to be specified.
     * @param schemeScriptString
     * @param currentURI
     * @return
     */
    public static Object executeStatic( Runnable threadInitializer, String schemeScriptString, String currentURI ) {
        try {
            SchemeResult result = new SchemeEvaluator().evaluate( threadInitializer, schemeScriptString , currentURI );
            result.throwIfError();
            return result.getValue(); 
        } catch (Throwable e) {
                SimpleConsole.getConsole().addText(e);
            logError( "Ignored an error : ", e );
            return null;
        }
    }

    
    public static Object executeStatic2( Runnable threadInitializer, String schemeScriptString, String currentURI ) {
        try {
            SchemeResult result = MultiplexEvaluator.createLocal().evaluate( threadInitializer, schemeScriptString , currentURI );
            result.throwIfError();
            return result.getValue(); 
        } catch (Throwable e) {
                SimpleConsole.getConsole().addText(e);
            logError( "Ignored an error : ", e );
            return null;
        }
    }
    
    /**
     * Resolve the given relative path to the absolute path with the <i>base-file</i> which
     * is determined by <i>currentBaseFile</i> with some conditions to mutate the given path.
     * This method implements a simple module system based on the directory system.
     * <p/>
     * This method has two conditions to determine when to mutate the given path.
     * <ul>
     * <li>If the given path does not exist, the method modifies the path to its parent directory.</li>
     * <li>If the given path is pointing a directory file, the method modifies the 
     * path to point to a file by appending the file-name `main.scm`.</li>
     * </ul>
     * This behavior effectively implements a simple module system based on the directory system;
     * consider the following situations:
     * <ul>
     * <li>
     * The main-module file is located in a directory and the other submodule files are placed to the same directory.<p/>
     * <pre>
     * -main.scm  
     *   +sub01.scm
     *   +sub02.scm
     * ---
     * main.scm:
     *   (use "sub01.scm")  
     *   (use "sub02.scm")  
     * </pre>
     * </li>
     * <li>
     * The main-module file is located in a directory and the other submodule files are placed to subdirectories.<p/>
     * <pre>
     * -main.scm  
     *   +sub01/main.scm
     *   +sub02/main.scm
     * ---
     * main.scm:
     *   (use "sub01")  
     *   (use "sub02")  
     * </pre>
     * </li>
     * <li>
     * There is a common module directory and the directory where the main-module is placed is placed to the common
     * module direcoty as well as the other submodule directories.    
     * <pre>
     * - common-modules 
     *   +main01/main.scm  
     *   +sub01/main.scm
     *   +sub02/main.scm
     * ---
     * main01/main.scm:
     *   (use "sub01")  
     *   (use "sub02")  
     * </pre>
     * </li>
     * </ul> 
     * <p/>
     * A module directory name should be unique in the entire module directory name set.
     * <p> 
     * @param file
     *    the file path to be resolved
     * @return
     *    the resolved file path
     */
    public static File useResolve( File file ) {
        return useResolveProc( SchemeEvaluator.getCurrentBaseFile(), file);
    }
    
    /**
     * The implementation part of {@link #useResolve(File)}. This method is intended to provide
     * tha way to share the algorithm of path resolving which is done in {@link #useResolve(File)}.  
     *  
     * @param baseFile
     *    the base file in which is to be resolved with
     * @param file
     *    the file path to be resolved.
     * @return
     *    the resolved file path
     */
    public static File useResolveProc( File baseFile, File file ) {
        if (baseFile==null)
            throw new IllegalArgumentException( "baseFile cannot be null" );
        if (file == null)
            throw new IllegalArgumentException( "file cannot be null" );
        
        File resolvedFile;
        if ( file.isAbsolute()  ) {
            resolvedFile = file;
        } else {
            resolvedFile = new File( baseFile.getParentFile(), file.getPath() );
            if ( ! resolvedFile.exists() ) {
                try {
                    // See the comment.
                    resolvedFile = new File( baseFile.getParentFile().getParentFile(), file.getPath() );
                } catch ( NullPointerException e ) {
                    logInfo( "could not get parent file (" + baseFile + ") " +  e.getMessage() );
                }
            }
        }
        
        // ADDED (Mon, 22 Jun 2020 13:47:21 +0900) >>>
        if ( resolvedFile.isDirectory() ) {
            resolvedFile = new File( resolvedFile, USE_MAINFILE );
        }
        // ADDED (Mon, 22 Jun 2020 13:47:21 +0900) <<<
        
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

    /**
     * Evaluate the specified file with the current context in the means of the state of ThreadManager object instances.
     * The specified file path is altered by some conditions. To learn how it alters the path, see {@link #useResolve(File)}.  
     * 
     * @param file
     *    specifies the file to evaluate
     * @return
     *    the result of the evaluation
     * @throws IOException
     * 
     * @see #useResolve
     */
    public static Object use( File file ) throws IOException {
        Evaluator evaluator = Evaluator.getCurrent();
        if ( evaluator == null ) {
            throw new IllegalStateException("NO EVALUATOR ERROR : currently, no evaluator was configured.");
        }
        
        ThreadManager threadManager = ThreadManager.getCurrent();
        if ( threadManager == null ) {
            throw new IllegalStateException("NO THREAD MANAGER ERROR : currently, no thread manager was configured.");
        }

        EvaluatorReceiverImplementation resultReceiver = new EvaluatorReceiverImplementation();
        File resolvedFile = useResolve(file);

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
    
    /**
     * Read the specified file as either a lisp's symbol list or a symbol. The given
     * filename will be resolved with the same algorithm as {@link #useResolve(File)}
     * method.
     * 
     * @param file
     *    the file to be read
     * @return  
     *    a symbol or a symbol list
     * @throws IOException
     */
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
