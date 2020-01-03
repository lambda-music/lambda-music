package pulsar.lib.scheme;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import gnu.expr.Language;
import gnu.mapping.Environment;
import gnu.mapping.Procedure0;
import kawa.standard.Scheme;
import kawa.standard.load;
import pulsar.lib.CurrentObject;
import pulsar.lib.app.ApplicationComponent;
import pulsar.lib.thread.ThreadInitializer;
import pulsar.lib.thread.ThreadInitializerCollection;
import pulsar.lib.thread.ThreadInitializerContainer;

public class SchemeEvaluator implements ThreadInitializerContainer<SchemeEvaluator>, ApplicationComponent {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }
    
    public abstract interface SchemeEngineListener {
        public abstract void execute( Scheme resource );
    }
    
    Scheme scheme=null;
    public SchemeEvaluator() {
    }
    public SchemeEvaluator( Scheme scheme ) {
        this.scheme = scheme;
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
                initializeCurrentThread( scheme );
            }
        });
        registerSchemeInitializer( new SchemeEngineListener() {
            @Override
            public void execute(Scheme scheme) {
                staticInitScheme( scheme );
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
     * {@link SchemeEvaluatorUtils#evaluateScheme(Scheme, Runnable, Reader, File, File, String)}  
     */
    public static void staticInitScheme( Scheme scheme ) {
        Environment env = scheme.getEnvironment();
        SchemeUtils.defineVar(env, load.loadRelative , "source" );
        SchemeUtils.defineVar(env, new Procedure0() {
            @Override
            public Object apply0() throws Throwable {
                return Language.getDefaultLanguage();
            }
        }, "current-scheme" );
        SchemeUtils.defineVar(env, new Procedure0() {
            @Override
            public Object apply0() throws Throwable {
                return Language.getDefaultLanguage();
            }
        }, "current-environment" );
    }

    public SchemeResult evaluate( 
            Runnable threadInitializer, 
            Reader schemeScript, 
            File currentDirectory, 
            File currentFile, 
            String currentURI )
    {
        return SchemeEvaluatorUtils.evaluateSchemeProc( 
            scheme,
            threadInitializer, 
            schemeScript, 
            currentDirectory, 
            currentFile, 
            currentURI );
    }

    // helper methods
    public SchemeResult evaluate( 
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

    public SchemeResult evaluate( Runnable threadInitializer, File schemeScriptFile ) throws IOException {
        return evaluate( 
            threadInitializer,
            new InputStreamReader( new FileInputStream( schemeScriptFile ) ), 
            schemeScriptFile.getParentFile(), 
            schemeScriptFile, 
            schemeScriptFile.getPath() 
            );
    }


    // resource
    public SchemeResult evaluate( Runnable threadInitializer, Class parentClass, String resourcePath ) throws IOException {
        return evaluate( 
            threadInitializer, 
            new InputStreamReader( parentClass.getResource( resourcePath ).openStream() ), 
            null, 
            null, 
            resourcePath 
            );
    }
    public SchemeResult evaluate( Class parentClass, String resourcePath ) throws IOException {
        return evaluate( null, parentClass, resourcePath ); 
    }
}
