package lamu.lib.scheme;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.lang.invoke.MethodHandles;
import java.util.logging.Level;

import lamu.lib.log.Logger;

public interface Evaluator {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }

    public static final ThreadLocal<Evaluator> threadLocal = new ThreadLocal<>();
    public static Evaluator getCurrent(){
        return threadLocal.get();
    }
    public static void setCurrent( Evaluator threadManager ){
        threadLocal.set( threadManager );
    }

    /**
     * This method defines how to valuate the given script. This implemantation may evaluate the given script in
     * the local environment, a remote server which can communicate by sockets, or other situation.
     *  
     * <i>
     * Note that the implementor of this method is oblidged to call the {@link Evaluator#setCurrentEvaluator()} method.
     * </i>
     * 
     * @param threadInitializer
     *    Specifies the thread initializers  
     * @param schemeScript
     *    The scheme script to be executed. 
     * @param currentDirectory
     *    This parameter is ignored.
     * @param currentFile
     *    Specifies the current executing file if it actually exists; this can be null if it does not exist.
     * @param currentURI
     *    Specifies the current executing file by URI; this can be any arbitrary value.
     *    This value will be used to report the current line number when any exception is thronw duruing the execution;
     *    that is, this can be practically any string value as long as it is clear that where the execption happens.
     * @return
     *    The {@link SchemeResult} object which contains the information of the result of the execution.  
     */
    abstract SchemeResult evaluate(
            Runnable threadInitializer, 
            Reader schemeScript, 
            File currentDirectory, 
            File currentFile,
            String currentURI );

    default void setCurrentEvaluator() {
        threadLocal.set(this);
    }
    
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

    /**
     * Initialize the current evaluator.
     */
    void initializeEvaluator();

    /**
     * Finalize the current evaluator.
     */
    void finalizeEvaluator();
}