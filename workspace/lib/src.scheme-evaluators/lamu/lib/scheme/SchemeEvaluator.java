package lamu.lib.scheme;

import java.io.File;
import java.io.Reader;
import java.lang.invoke.MethodHandles;
import java.util.logging.Level;

import gnu.expr.Language;
import gnu.mapping.Environment;
import kawa.standard.Scheme;
import lamu.lib.CurrentObject;
import lamu.lib.app.ApplicationComponent;
import lamu.lib.log.Logger;
import lamu.lib.thread.ThreadInitializer;
import lamu.lib.thread.ThreadInitializerCollection;
import lamu.lib.thread.ThreadInitializerContainer;

public class SchemeEvaluator implements ThreadInitializerContainer<SchemeEvaluator>, ApplicationComponent, Evaluator, NameCaptionHolder {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }
    
    public abstract interface SchemeEngineListener {
        public abstract void execute( Scheme scheme );
    }
    
    final Scheme scheme;
    final String name;
    public SchemeEvaluator() {
        this.scheme = new Scheme();
        this.name = "local";
    }
    @Deprecated
    public SchemeEvaluator( Scheme scheme ) {
        this.scheme = scheme;
        this.name = "local";
    }
    
    //////////////////////////////////////////////////////////////////////////////////////////
    //
    //////////////////////////////////////////////////////////////////////////////////////////
    
    private static final CurrentObject<SchemeEvaluator> currentObject = new CurrentObject<>( SchemeEvaluator.class );
    private final ThreadInitializer<SchemeEvaluator> threadInitializer =
            ThreadInitializer.createMultipleThreadInitializer( "scheme", this,
                ThreadInitializer.createThreadInitializer( "current-scheme", currentObject, this ), 
                new Runnable() {
                    @Override
                    public void run() {
                        initializeCurrentThread( SchemeEvaluator.this.getScheme() );
                    }
                    @Override
                    public String toString() {
                        return "scheme-current-thread";
                    }
                });
    
    @Override
    public ThreadInitializer<SchemeEvaluator> getThreadInitializer() {
        return threadInitializer;
    }
    public static SchemeEvaluator getCurrent() {
        return currentObject.get();
    }
    public static boolean isPresent() {
        return currentObject.isPresent();
    }

    private final ThreadInitializerCollection defaultInitializerCollection = new ThreadInitializerCollection( "default-scheme", this );
    {
        defaultInitializerCollection.addThreadInitializer( getThreadInitializer() );
    }
    public ThreadInitializerCollection getDefaultInitializerCollection() {
        return defaultInitializerCollection;
    }

    //////////////////////////////////////////////////////////////////////////////////////////
    //
    //////////////////////////////////////////////////////////////////////////////////////////

    private ApplicationComponent parentApplicationComponent;
    @Override
    public ApplicationComponent getParentApplicationComponent() {
        return this.parentApplicationComponent;
    }
    @Override
    public void setParentApplicationComponent(ApplicationComponent parentApplicationComponent) {
        this.parentApplicationComponent = parentApplicationComponent;
    }

    @Override
    public void processInit() {
    }
    @Override
    public void processQuit() {
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

    @Override
    public SchemeResult evaluate( 
            Runnable threadInitializer, 
            Reader schemeScript, 
            File currentDirectory, 
            File currentFile, 
            String currentURI )
    {
        return SchemeEvaluatorImplementation.evaluateSchemeProc( 
            scheme,
            threadInitializer, 
            schemeScript, 
            currentDirectory, 
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
}
