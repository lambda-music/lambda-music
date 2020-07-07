package lamu.lib.evaluators;

import java.io.File;
import java.io.Reader;
import java.lang.invoke.MethodHandles;
import java.util.logging.Level;

import gnu.expr.Language;
import gnu.mapping.Environment;
import kawa.standard.Scheme;
import lamu.lib.logging.Logger;

public class SchemeEvaluator implements Evaluator, NameCaptionHolder {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }

    
    /**
     * This method returns the current base file.
     * See {@link SchemeEvaluatorImplementation#currentBaseFile } for further information.
     * @return
     *    the path to the current base-file
     */
    public static File getCurrentBaseFile() {
        File currentFile = SchemeEvaluatorImplementation.currentBaseFile.get();
        if ( currentFile == null ) {
            throw new IllegalStateException( 
                "NO BASE FILE DEFINED ERROR : " +
                "Currently no base file is defined. Please save the current editting file, before loading files." );
        }
        return currentFile;
    }

    public abstract interface SchemeEngineListener {
        public abstract void execute( Scheme scheme );
    }
    
    final Scheme scheme;
    final String name;
    public SchemeEvaluator() {
        this.scheme = new Scheme();
        this.name = "local";
    }
    public SchemeEvaluator(Scheme scheme, String name) {
        super();
        this.scheme = scheme;
        this.name = name;
    }

    public Scheme getScheme() {
        return this.scheme;
    }
    
    //////////////////////////////////////////////////////////////////////////////////////////
    // Thread Initializer
    //////////////////////////////////////////////////////////////////////////////////////////
    public static final void initializeCurrentThread( Scheme scheme ) {
        Language.setCurrentLanguage( scheme );
        Environment.setCurrent( scheme.getEnvironment() );
    }
    private final Runnable privateThreadInitializer = new Runnable() {
        @Override
        public void run() {
            initializeCurrentThread( SchemeEvaluator.this.getScheme() );
        }
    };

    @Override
    public SchemeResult evaluate( 
            Runnable threadInitializer, 
            Reader schemeScript, 
            File currentFile, 
            String currentURI )
    {
        this.setCurrentEvaluator();
        return SchemeEvaluatorImplementation.evaluateSchemeProc( 
            scheme,
            () -> {
                try {
                    if ( privateThreadInitializer!= null )
                        privateThreadInitializer.run(); 
                } catch ( Throwable e ) {
                    logError("", e);
                }
                try {
                    if ( threadInitializer!= null )
                        threadInitializer.run(); 
                } catch ( Throwable e ) {
                    logError("", e);
                }
            },
            schemeScript, 
            currentFile, 
            currentURI );
    }
    
    @Override
    public String toString() {
        return this.name;
    }
    @Override
    public String getNameCaption() {
        return this.name;
    }
    @Override
    public void initializeEvaluator() {
    }
    @Override
    public void finalizeEvaluator() {
    }
}
