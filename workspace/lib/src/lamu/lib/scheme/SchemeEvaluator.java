package lamu.lib.scheme;

import java.io.File;
import java.io.Reader;
import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import gnu.expr.Language;
import gnu.mapping.Environment;
import kawa.standard.Scheme;
import kawa.standard.load;
import lamu.lib.CurrentObject;
import lamu.lib.app.ApplicationComponent;
import lamu.lib.log.LamuLogger;
import lamu.lib.scheme.proc.MultipleNamedProcedure0;
import lamu.lib.thread.ThreadInitializer;
import lamu.lib.thread.ThreadInitializerCollection;
import lamu.lib.thread.ThreadInitializerContainer;

public class SchemeEvaluator implements ThreadInitializerContainer<SchemeEvaluator>, ApplicationComponent, Evaluator, HasName {
    static final Logger LOGGER = LamuLogger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }
    
    public abstract interface SchemeEngineListener {
        public abstract void execute( Scheme scheme );
    }
    
    Scheme scheme=null;
    String name=null;
    public SchemeEvaluator() {
        this.scheme = null;
        this.name = "local";
    }
    public SchemeEvaluator( Scheme scheme ) {
        this.scheme = scheme;
        this.name = "local";
    }
    public SchemeEvaluator( Scheme scheme, String name ) {
        this.scheme = scheme;
        this.name = name;
    }
    
    @Override
    public String getName() {
        return this.name;
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
        this.newScheme();
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

    //////////////////////////////////////////////////////////////////////////////////////////
    // Scheme Initializer 
    //////////////////////////////////////////////////////////////////////////////////////////
    
    private List<SchemeEngineListener> schemeInitializerList = new ArrayList<>();
    public List<SchemeEngineListener> getSchemeInitializerList() {
        return schemeInitializerList;
    }

    /**
     * This method registers a specified initializer.
     * @see #invokeSchemeInitializers()
     */
    public void registerSchemeInitializer( SchemeEngineListener schemeEngineListener ) {
        this.getSchemeInitializerList().add( schemeEngineListener  );
    }

    {
        registerSchemeInitializer( new SchemeEngineListener() {
            @Override
            public void execute(Scheme scheme) {
                // 3. This initializes Secretary Message Queue's thread.
                // // 4. (in most case ) this initializes main-thread
                SchemeEvaluator.initializeCurrentThread( scheme );
            }
        });
        registerSchemeInitializer( new SchemeEngineListener() {
            @Override
            public void execute(Scheme scheme) {
                SchemeEvaluator.initScheme( scheme );
            }
        });
    }
    
    private void invokeSchemeInitializers() {
        for ( SchemeEngineListener e : getSchemeInitializerList() ) {
            e.execute( this.scheme );
        }
    }

    private void newSchemeProc() {
        logInfo( "SchemeSecretary#newScheme()" );
        this.scheme = new Scheme();
    }

    public void newScheme() {
        try {
            // 1. Create a new scheme object.
            newSchemeProc();
            
            // 2. Execute all the initializers.
            invokeSchemeInitializers();
        } catch (Throwable e) {
            throw new RuntimeException(e);
        }
    }
    
    /**
     * "loadRelative" was moved from 
     * {@link SchemeEvaluatorImplementation#evaluateScheme(Scheme, Runnable, Reader, File, File, String)}  
     */
    public static void initScheme( Scheme scheme ) {
        Environment env = scheme.getEnvironment();
        SchemeUtils.defineVar(env, load.loadRelative , "source" );
        SchemeUtils.defineLambda(env, new MultipleNamedProcedure0( "current-scheme" ) {
            @Override
            public Object apply0() throws Throwable {
                return Language.getDefaultLanguage();
            }
        });
        SchemeUtils.defineLambda(env, new MultipleNamedProcedure0( "current-environment" ) {
            @Override
            public Object apply0() throws Throwable {
                return Language.getDefaultLanguage();
            }
        });

//        SchemeUtils.defineVar(env, new Procedure0() {
//            @Override
//            public Object apply0() throws Throwable {
//                SchemeUtils.getAllKey( (Scheme)Language.getDefaultLanguage() );
//                return 
//            }
//        }, "get-all-identifiers" );
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
    public void reset() {
        this.newScheme();
    }
}
