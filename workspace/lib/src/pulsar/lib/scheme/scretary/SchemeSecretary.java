package pulsar.lib.scheme.scretary;

import java.io.File;
import java.io.Reader;
import java.io.StringReader;
import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
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
import pulsar.lib.scheme.SchemeExecutorUtils;
import pulsar.lib.scheme.SchemeResult;
import pulsar.lib.scheme.SchemeUtils;
import pulsar.lib.secretary.Invokable;
import pulsar.lib.secretary.InvokablyRunnable;
import pulsar.lib.secretary.SecretaryMessage;
import pulsar.lib.thread.ThreadInitializer;
import pulsar.lib.thread.ThreadInitializerCollection;
import pulsar.lib.thread.ThreadInitializerContainer;

public class SchemeSecretary implements ThreadInitializerContainer<SchemeSecretary>, ApplicationComponent {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }
    
    Scheme scheme=null;
    public SchemeSecretary() {
    }
    
    public Invokable createSecretarillyInvokable( Procedure procedure ) {
        return new InvokableSchemeProcedure( procedure );  
//      return new SecretariallyInvokable( this, new InvokableSchemeProcedure( procedure ) );  
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
    
    Scheme getScheme() {
        return getExecutive();
    }
    
    //////////////////////////////////////////////////////////////////////////////////////////
    // Thread Initializer
    //////////////////////////////////////////////////////////////////////////////////////////
    public static final void initializeCurrentThread( Scheme scheme ) {
        Language.setCurrentLanguage( scheme );
        Environment.setCurrent( scheme.getEnvironment() );
    }
    public final void initializeCurrentThread() {
        initializeCurrentThread( this.getScheme() );
    }

    //////////////////////////////////////////////////////////////////////////////////////////
    // Scheme Initializer 
    //////////////////////////////////////////////////////////////////////////////////////////
    
    private List<SecretaryMessage.NoReturnNoThrow<Scheme>> schemeInitializerList = new ArrayList<>();
    public List<SecretaryMessage.NoReturnNoThrow<Scheme>> getSchemeInitializerList() {
        return schemeInitializerList;
    }

    /**
     * This method registers a specified initializer.
     * @see #invokeSchemeInitializers()
     */
    public void registerSchemeInitializer( SecretaryMessage.NoReturnNoThrow<Scheme> message ) {
        this.getSchemeInitializerList().add( message  );
    }

    public void invokeSchemeInitializers() {
        for ( SecretaryMessage.NoReturnNoThrow<Scheme> e : getSchemeInitializerList() ) {
            executeSecretarially( e );
        }
    }

    {
        registerSchemeInitializer( new SecretaryMessage.NoReturnNoThrow<Scheme>() {
            @Override
            public void execute0(Scheme scheme, Object[] args) {
                staticInitScheme( scheme );
            }
        });
        registerSchemeInitializer( new SecretaryMessage.NoReturnNoThrow<Scheme>() {
            @Override
            public void execute0(Scheme scheme, Object[] args) {
                // 3. This initializes Secretary Message Queue's thread.
                // // 4. (in most case ) this initializes main-thread
                initializeCurrentThread( scheme );
            }
        });
        registerSchemeInitializer( new SecretaryMessage.NoReturnNoThrow<Scheme>() {
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
        
        registerSchemeInitializer( new SecretaryMessage.NoReturnNoThrow<Scheme>() {
            @Override
            public void execute0(Scheme scheme, Object[] args) {
                initializeCurrentThread( scheme );
                //XXX This destroys the thread initialization for Environment().
                // (Thu, 15 Aug 2019 23:07:01 +0900)
//              SchemeUtils.defineVar( scheme, EmptyList.emptyList, "all-procedures" );
            }
        });
    }

    public Scheme getExecutive() {
        return this.scheme;
    }
    public void setExecutive(Scheme newScheme) {
        this.scheme = newScheme;
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


    public void newScheme() {
        try {
            
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
            invokeSchemeInitializers();
            
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
                return SchemeExecutorUtils.evaluateScheme(scheme, threadInitializer, schemeScript, currentDirectory, schemeScriptFile, schemeScriptURI);
            }
        }, Invokable.NOARG );
    }
    
    public <R,T,E extends Throwable> T executeSecretarially( SecretaryMessage<Scheme,T,E> message, Object... args ) throws E {
        return message.execute( getExecutive(), args );
    }

    /**
     * "loadRelative" was moved from 
     * {@link SchemeExecutorUtils#evaluateScheme(Scheme, Runnable, Reader, File, File, String)}  
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
}
