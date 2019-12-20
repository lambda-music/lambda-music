package pulsar.lib.scheme.scretary;

import java.io.File;
import java.io.Reader;
import java.io.StringReader;
import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.SwingUtilities;

import gnu.expr.Language;
import gnu.mapping.Environment;
import gnu.mapping.Procedure;
import gnu.mapping.Procedure0;
import kawa.standard.Scheme;
import kawa.standard.load;
import pulsar.lib.CurrentObject;
import pulsar.lib.app.ApplicationComponent;
import pulsar.lib.scheme.SchemeExecutor;
import pulsar.lib.scheme.SchemeResult;
import pulsar.lib.scheme.SchemeUtils;
import pulsar.lib.secretary.Invokable;
import pulsar.lib.secretary.InvokablyRunnable;
import pulsar.lib.secretary.SecretariallyInvokable;
import pulsar.lib.secretary.Secretary;
import pulsar.lib.secretary.SecretaryMessage;
import pulsar.lib.thread.ThreadInitializer;
import pulsar.lib.thread.ThreadInitializerCollection;
import pulsar.lib.thread.ThreadInitializerContainer;

public class SchemeSecretary extends Secretary<Scheme> implements ThreadInitializerContainer<SchemeSecretary>, ApplicationComponent {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }
    public SchemeSecretary() {
    }
    public Invokable createSecretarillyInvokable( Procedure procedure ) {
        return new SecretariallyInvokable( this, new InvokableSchemeProcedure( procedure ) );  
    }
    public Runnable createRunnableAndInvocable( Procedure procedure, Object... args) {
        return new InvokablyRunnable( this.createSecretarillyInvokable( procedure ), args );
    }

    //////////////////////////////////////////////////////////////////////////////////////////
    //
    //////////////////////////////////////////////////////////////////////////////////////////
    
    private static final CurrentObject<SchemeSecretary> currentObject = new CurrentObject<>( SchemeSecretary.class );
    private final ThreadInitializer<SchemeSecretary> threadInitializer =
            ThreadInitializer.createMultipleThreadInitializer( "scheme", this,
                ThreadInitializer.createThreadInitializer( "current-scheme", currentObject, this ), new Runnable() {
                    @Override
                    public void run() {
                        initializeCurrentThread();
                    }
                    @Override
                    public String toString() {
                        return "scheme-current-thread";
                    }
                });
    @Override
    public ThreadInitializer<SchemeSecretary> getThreadInitializer() {
        return threadInitializer;
    }
    public static SchemeSecretary getCurrent() {
        return currentObject.get();
    }

    final ThreadInitializerCollection defaultInitializerCollection = new ThreadInitializerCollection( "default-scheme", this );
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
    public void requesetInit() {
        this.newScheme();
    }
    @Override
    public void requestShutdown() {
    }
    
    Scheme getScheme() {
        return getExecutive();
    }
    
    //////////////////////////////////////////////////////////////////////////////////////////
    // SHUTDOWN HOOK
    //////////////////////////////////////////////////////////////////////////////////////////
    private final Collection<Runnable> shutdownHookList = new LinkedList<>();
    public void addShutdownHook( Runnable runnable ) {
        synchronized ( this.shutdownHookList ) {
            this.shutdownHookList.add( runnable );
        }
    }
    public void removeShutdownHook( Runnable runnable ) {
        synchronized ( this.shutdownHookList ) {
            this.shutdownHookList.remove( runnable );
        }
    }
    public void executeShutdownHook() {
        synchronized ( this.shutdownHookList ) {
            for ( Runnable r : this.shutdownHookList ) {
                try {
                    r.run();
                } catch ( Throwable e ) {
                    logError("", e);
                }
            }
        }
    }
    
    //////////////////////////////////////////////////////////////////////////////////////////
    // SHUTDOWN HOOK
    //////////////////////////////////////////////////////////////////////////////////////////
    public static final void initializeCurrentThread( Scheme scheme ) {
        Language.setCurrentLanguage( scheme );
        Environment.setCurrent( scheme.getEnvironment() );
    }
    public final void initializeCurrentThread() {
        initializeCurrentThread( this.getScheme() );
    }
    
    private final Collection<Runnable> threadInitializerList = new LinkedList<>();
    public void registerThreadInitializer( Runnable initializer ) {
        threadInitializerList.add( initializer );
    }
    public void unregisterThreadInitializer( Runnable initializer ) {
        threadInitializerList.remove( initializer );
    }
    public Collection<Runnable> getThreadInitializerList() {
        return threadInitializerList;
    }
    
    //////////////////////////////////////////////////////////////////////////////////////////
    // Scheme Initializer 
    //////////////////////////////////////////////////////////////////////////////////////////

    /*
     * Scheme Initializer initializes the new Scheme instance when the SchemeSecretary creates 
     * a new Scheme instance.  
     */
    static class InitializerEntry {
        Object parent;
        SecretaryMessage.NoReturnNoThrow<Scheme> message;
        public InitializerEntry(Object parent, SecretaryMessage.NoReturnNoThrow<Scheme> message) {
            super();
            this.parent = parent;
            this.message = message;
        }
    }
    
    private List<SchemeSecretary.InitializerEntry> schemeInitializerList = new ArrayList<>();
    public List<SchemeSecretary.InitializerEntry> getSchemeInitializerList() {
        return schemeInitializerList;
    }

    /**
     * This method registers a specified initializer.
     * @see #invokeSchemeInitializers(Object)
     */
    public void registerSchemeInitializer( Object parent, SecretaryMessage.NoReturnNoThrow<Scheme> message ) {
        this.getSchemeInitializerList().add( new SchemeSecretary.InitializerEntry( parent, message ) );
    }
    /**
     * This method unregisters a specified initializer.
     * @see #invokeSchemeInitializers(Object)
     */
    public void unregisterSchemeInitializer( Object parent ) {
        this.getSchemeInitializerList().removeIf( e->e.parent == parent );
    }

    /**
     * This method invokes registered initializers to which the given condition match.
     *  
     * @param parent
     *     Passing null to invoke all initializers or passing parent object to invoke 
     *     only specific initializers. 
     * 
     *     If the parent argument equals to the parent of a registered initializer, the 
     *     initializer will be invoked. If the parent argument is null, all initializers
     *     will be invoked.
     */
    public void invokeSchemeInitializers( Object parent ) {
        for ( SchemeSecretary.InitializerEntry e : getSchemeInitializerList() ) {
            if ( parent == null || e.parent == parent )
                executeSecretarially( e.message );
        }
    }

    {
        registerSchemeInitializer( null, new SecretaryMessage.NoReturnNoThrow<Scheme>() {
            @Override
            public void execute0(Scheme scheme, Object[] args) {
                staticInitScheme( scheme );
            }
        });
        registerSchemeInitializer( null, new SecretaryMessage.NoReturnNoThrow<Scheme>() {
            @Override
            public void execute0(Scheme scheme, Object[] args) {
                // 3. This initializes Secretary Message Queue's thread.
                // // 4. (in most case ) this initializes main-thread
                initializeCurrentThread( scheme );
            }
        });
        registerSchemeInitializer( null, new SecretaryMessage.NoReturnNoThrow<Scheme>() {
            @Override
            public void execute0(Scheme scheme, Object[] args) {
                SwingUtilities.invokeLater( new Runnable() {
                    @Override
                    public void run() {
                        // 5. This initializes AWT-EventQueue's thread.
                        initializeCurrentThread(scheme);
                    }
                });
            }
        });
        
        registerSchemeInitializer( null, new SecretaryMessage.NoReturnNoThrow<Scheme>() {
            @Override
            public void execute0(Scheme scheme, Object[] args) {
                initializeCurrentThread( scheme );
                //XXX This destroys the thread initialization for Environment().
                // (Thu, 15 Aug 2019 23:07:01 +0900)
//              SchemeUtils.defineVar( scheme, EmptyList.emptyList, "all-procedures" );
            }
        });
    }

    @Override
    public Scheme getExecutive() {
        // TODO Auto-generated method stub
        return super.getExecutive();
    }

    static class FinalizerEntry {
        Object parent;
        SecretaryMessage.NoReturnNoThrow<Scheme> message;
        public FinalizerEntry(Object parent, SecretaryMessage.NoReturnNoThrow<Scheme> message) {
            super();
            this.parent = parent;
            this.message = message;
        }
    }

    List<SchemeSecretary.FinalizerEntry> newSchemeFinalizerList = new ArrayList<>();
    
    /**
     * This method registers a specified finalizer.
     * @see #invokeSchemeFinalizer(Object)
     */
    public void registerSchemeFinalizer( Object parent, SecretaryMessage.NoReturnNoThrow<Scheme> message ) {
        this.newSchemeFinalizerList.add( new SchemeSecretary.FinalizerEntry( parent, message ) );
    }
    /**
     * This method unregisters a specified finalizer.
     * @see #invokeSchemeFinalizer(Object)
     */
    public void unregisterSchemeFinalizer( Object parent ) {
        this.newSchemeFinalizerList.removeIf( e->e.parent == parent );
    }

    public void invokeSchemeFinalizer( Object parent ) {
        for ( SchemeSecretary.FinalizerEntry e : newSchemeFinalizerList ) {
            if ( parent == null || e.parent == parent )
                executeSecretarially( e.message );
        }
    }


    /*-
     *  A memorandum (Wed, 24 Jul 2019 09:43:02 +0900) 
     *
     *  Yesterday I could not correctly imagine what the scheme initialization
     *  on the Pulsar system is supposed to be. Yesterday I categorized those
     *  many initializers into two : dynamic/static but the way to categorize
     *  did not seem to be proper. It did not work properly in my mind and I
     *  could not figure out how.
     * 
     *  This morning I realized that I am struggling with the objects which
     *  have totally uneven life cycle and each of those objects supposed to
     *  have two kind of initializers.
     * 
     *  There are some types of objects in an instance of Pulsar application.
     * 
     *  1. The scheme object 
     *  2. Pulsar's  Frame objects (global/local)
     *  3. Kawapad's Frame objects (global/local)
     * 
     *  |                                          |                                        |
     *  |                                          |                                        |
     *  |<===== 1. SCHEME OBJECT LIFE SPAN =======>|<===== 1. SCHEME OBJECT LIFE SPAN =====>|
     *  |                                          |                                        |
     *  |       |<=== 2.FRAME LIFE SPAN ===>|      |  |<==== 2. FRAME LIFE SPAN ====>|      |
     *  |   |<=== 3.FRAME LIFE SPAN ===>|   |<==== 3. FRAME LIFE SPAN ====>|                |
     *  |                                          |                                        |
     *  
     * 
     *  1. Whenever scheme object is renewed, every initializer including
     *     frame's initializers must be invoked.
     *
     *  2. Whenever a new frame is created, some new initializers must be
     *     registered and invoked at the same time.
     *
     *  3. The frame's initializers must be removed when the frame is disposed.
     *
     *  4.  When the current scheme object is destroyed and created a new
     *      scheme object, we have to initialize the object again.
     *
     *  5.  Some of initializers are shared between all instance, and others are not shared
     *      and tied to a single instance of frame.
     *
     *  We categorize these initializers into two groups :
     *
     *    1. Global Initializer
     *    2. Local Initializer
     * 
     * The problem is, how we create the first scheme object and initialize it.
     * 
     *
     *
     * This memo is not completed. This is left for future reference.
     * (Wed, 24 Jul 2019 10:10:41 +0900)
    */

//  static Environment env=null;
    public void newScheme() {
        try {

            // 0. Create a new scheme object.
            executeSecretarially( new SecretaryMessage.NoReturnNoThrow<Scheme>() {
                @Override
                public void execute0(Scheme scheme, Object[] args) {
                    logInfo( "SchemeSecretary#newScheme()" );
                    if ( scheme != null ) {
                        // 0. Execute all the initializers.
                        invokeSchemeFinalizer( null );
                    }
                }
            });
            
            
            // 1. Create a new scheme object.
            executeSecretarially( new SecretaryMessage.NoReturnNoThrow<Scheme>() {
                @Override
                public void execute0(Scheme scheme, Object[] args) {
                    logInfo( "SchemeSecretary#newScheme()" );
                    Scheme newScheme = new Scheme();
                    setExecutive( newScheme );
//                  env = newScheme.getEnvironment();
                }
            });
            // 2. Execute all the initializers.
            invokeSchemeInitializers( null );
        } catch (Throwable e) {
            throw new RuntimeException(e);
        }
    }
    
    void setScheme( Scheme newScheme ) {
        try {
            executeSecretarially( new SecretaryMessage.NoReturnNoThrow<Scheme>() {
                @Override
                public void execute0(Scheme resource, Object[] args) {
                    setExecutive( newScheme );
                }
            });
        } catch (Throwable e) {
            throw new RuntimeException(e);
        }
    }
    public static SchemeResult evaluateScheme( 
            SchemeSecretary schemeSecretary, Runnable threadInitializer, 
            String schemeScript, File currentDirectory, File schemeScriptFile, String schemeScriptURI ) 
    {
        return SchemeSecretary.evaluateScheme(
            schemeSecretary, threadInitializer,    
            new StringReader( schemeScript ), currentDirectory, schemeScriptFile, schemeScriptURI );
    }

    public static SchemeResult evaluateScheme( 
            SchemeSecretary schemeSecretary, Runnable threadInitializer, 
            Reader schemeScript, File currentDirectory, File schemeScriptFile, String schemeScriptURI ) {
        return schemeSecretary.executeSecretarially( new SecretaryMessage.NoThrow<Scheme,SchemeResult>() {
            @Override
            public SchemeResult execute0(Scheme scheme, Object[] args) {
                return SchemeExecutor.evaluateScheme(scheme, threadInitializer, schemeScript, currentDirectory, schemeScriptFile, schemeScriptURI);
            }
        }, Invokable.NOARG );
    }
    
    /**
     * "loadRelative" was moved from 
     * {@link SchemeExecutor#evaluateScheme(Scheme, Runnable, Reader, File, File, String)}  
     */
    public static void staticInitScheme( Scheme scheme ) {
        Environment env = scheme.getEnvironment();

        // THIS IS A COPY FROM SchemeExecutor#evaluateScheme 
        // // I feel overriding "load" by "load-relative" is too risky. It
        // // may destroy the compatibility inside the kawa library; we
        // // decide to call it "source".  Usually ,this kind of
        // // initialization process should be done in staticInitScheme()
        // // method.  But we want to make it visible here that "source"
        // // is available in this way.  (Mon, 09 Sep 2019 04:31:19 +0900)
        // SchemeUtils.defineVar(env, load.loadRelative , "source" );
        // Moved from SchemeExecutor (Thu, 19 Dec 2019 02:43:01 +0900)
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
}
