package pulsar.lib.scheme.scretary;

import java.io.File;
import java.io.Reader;
import java.io.StringReader;
import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.SwingUtilities;

import gnu.expr.Language;
import gnu.mapping.Environment;
import gnu.mapping.Procedure;
import kawa.standard.Scheme;
import pulsar.lib.scheme.SchemeUtils;
import pulsar.lib.secretary.Invokable;
import pulsar.lib.secretary.SecretariallyInvokable;
import pulsar.lib.secretary.Secretary;
import pulsar.lib.secretary.SecretaryMessage;

public class SchemeSecretary extends Secretary<Scheme> implements ShutdownHook {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }
    public SchemeSecretary() {
    }
    public Invokable createSecretarillyInvokable( Procedure procedure ) {
        return new SecretariallyInvokable( this, new InvokableSchemeProcedure( procedure ) );  
    }
    Scheme getScheme() {
        return getExecutive();
    }
    
    /*-
     * Memorandum 1 (Someday in 2018)
     * 
     * This field specifies an environment object which contains a scope of scheme
     * language to be executed with.
     * 
     * I think the necessity of this field is needed to be explained.
     * 
     * Usually these environment objects are not supposed to be set manually. The
     * reason why it to be set manually is that sometimes the environment object is
     * missing. I am still not sure what exactly condition causes it to be gone, but
     * it occurs sometimes.
     * 
     * It seemed to me that Kawa's environment objects are stored per a thread. I am
     * still not sure when they are created. But when multiple threads access to a
     * single Kawa scheme object, sometime the environment object on the scheme
     * object is gone and this phenomenon causes strange errors.
     * 
     * A working around which I found is setting the desired environment object
     * before calling the invokable.
     * 
     * This field keeps the environment object which is in charge when the invokable
     * (closure) was passed in order to set it it every time before calling the
     * invokable.
     * 
     * I am quite sure this is not proper way to solve the problem, but it worked
     * for me.
     */

    
    /*-
     * Memorandum 2 (Sat, 20 Jul 2019 11:33:56 +0900)
     * 
     * When a Scheme invokable is called from SwingUtilities#invokeLater(),
     * NullPointerException occurs sometimes. The stack trace is following : 
     *
     * <pre>
     *   java.lang.NullPointerException
     *   at gnu.expr.PrimProcedure.decodeType(PrimProcedure.java:404)
     *   at gnu.expr.PrimProcedure.<init>(PrimProcedure.java:371)
     *   at gnu.kawa.reflect.ClassMethods.getMethods(ClassMethods.java:136)
     *   at gnu.kawa.reflect.ClassMethods.apply(ClassMethods.java:252)
     *   at gnu.kawa.functions.GetNamedPart.getMemberPart(GetNamedPart.java:120)
     *   at gnu.kawa.functions.GetNamedPart.getNamedPart(GetNamedPart.java:106)
     *   at gnu.kawa.functions.GetNamedPart.apply2(GetNamedPart.java:46)
     *   at atInteractiveLevel-93$frame.lambda14(pulsar-basic-framework.scm:612)
     * </pre>
     *  The reason this NPE occurs is the passed "lang" argument sometimes becomes null
     *
     * <pre>
     *      public static Type decodeType(Type javaType,
     *                                    String[] annotTypes, int annotIndex,
     *                                        ParameterizedType parameterizedType,
     *                                        Language lang) {
     *              String annotType = annotTypes != null && annotTypes.length > annotIndex
     *                  ? annotTypes[annotIndex] : null;
     *              return lang.decodeType(javaType, annotType, parameterizedType);
     *          }
     * </pre>
     * 
     * Then I found that setting current language by Language.setCurrentLanguage( language ) 
     * effectively suppresses the NPE.
     *  
     * I am pretty sure this is not supposed to be. 
     * But I don't know how to manage this problem for some years. 
     */

    public static final void initializeSchemeForCurrentThreadStatic( Scheme scheme ) {
        Language.setCurrentLanguage( scheme );
        Environment.setCurrent( scheme.getEnvironment() );
//      logInfo( "initializeSchemeForCurrentThreadStatic:" +  Environment.getCurrent().getName() );
        if ( false ) {
            String threadName = Thread.currentThread().getName();
            logInfo( threadName +
                "(Environment.getCurrent() == newScheme.getEnvironment() ) : " + 
                ( Environment.getCurrent() == scheme.getEnvironment() ) 
            );
            logInfo( threadName +
                "( Language.getDefaultLanguage() == newScheme ) : " + 
                ( Language.getDefaultLanguage() == scheme ) 
                    );
//          logInfo( threadName +
//              "(Environment.getCurrent() == env ) : " + 
//              ( Environment.getCurrent() == env ) 
//          );
        }
        // !!! THIS IS VERY BOGUS !!!! (Thu, 15 Aug 2019 22:38:07 +0900)
        // SEE gnu.mapping.RunnableClosure.run()
//      CallContext.getInstance();
    }
    private final Collection<Runnable> shutdownHookList = new LinkedList<>();
    @Override
    public Collection<Runnable> getShutdownHookList() {
        return shutdownHookList;
    }
    
    public final void initializeSchemeForCurrentThread() {
        initializeSchemeForCurrentThreadStatic( this.getScheme() );
    }
    
    static class InitializerEntry {
        Object parent;
        SecretaryMessage.NoReturnNoThrow<Scheme> message;
        public InitializerEntry(Object parent, SecretaryMessage.NoReturnNoThrow<Scheme> message) {
            super();
            this.parent = parent;
            this.message = message;
        }
    }
    
    List<SchemeSecretary.InitializerEntry> newSchemeInitializerList = new ArrayList<>();
    
    /**
     * This method registers a specified initializer.
     * @see #invokeSchemeInitializers(Object)
     */
    public void registerSchemeInitializer( Object parent, SecretaryMessage.NoReturnNoThrow<Scheme> message ) {
        this.newSchemeInitializerList.add( new SchemeSecretary.InitializerEntry( parent, message ) );
    }
    /**
     * This method unregisters a specified initializer.
     * @see #invokeSchemeInitializers(Object)
     */
    public void unregisterSchemeInitializer( Object parent ) {
        this.newSchemeInitializerList.removeIf( e->e.parent == parent );
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
        for ( SchemeSecretary.InitializerEntry e : newSchemeInitializerList ) {
            if ( parent == null || e.parent == parent )
                executeSecretarially( e.message );
        }
    }

    {
        registerSchemeInitializer( null, new SecretaryMessage.NoReturnNoThrow<Scheme>() {
            @Override
            public void execute0(Scheme scheme, Object[] args) {
                // 3. This initializes Secretary Message Queue's thread.
                // // 4. (in most case ) this initializes main-thread
                initializeSchemeForCurrentThreadStatic( scheme );
            }
        } );

        registerSchemeInitializer( null, new SecretaryMessage.NoReturnNoThrow<Scheme>() {
            @Override
            public void execute0(Scheme scheme, Object[] args) {
                SwingUtilities.invokeLater( new Runnable() {
                    @Override
                    public void run() {
                        // 5. This initializes AWT-EventQueue's thread.
                        initializeSchemeForCurrentThreadStatic(scheme);
                    }
                });
            }
        });
        
        registerSchemeInitializer( null, new SecretaryMessage.NoReturnNoThrow<Scheme>() {
            @Override
            public void execute0(Scheme scheme, Object[] args) {
                initializeSchemeForCurrentThreadStatic( scheme );
                //XXX This destroys the thread initialization for Environment().
                // (Thu, 15 Aug 2019 23:07:01 +0900)
//              SchemeUtils.defineVar( scheme, EmptyList.emptyList, "all-procedures" );
            }
        } );
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
    public static SchemeUtils.ExecuteSchemeResult evaluateScheme( SchemeSecretary schemeSecretary,
            Collection<Runnable> threadInitializers, 
            Map<String,Object> variables, 
            String schemeScript, File currentDirectory, File schemeScriptFile, String schemeScriptURI ) 
    {
        return SchemeSecretary.evaluateScheme(
            schemeSecretary, threadInitializers, variables,  
            new StringReader( schemeScript ), currentDirectory, schemeScriptFile, schemeScriptURI );
    }

    public static SchemeUtils.ExecuteSchemeResult evaluateScheme( SchemeSecretary schemeSecretary, 
            Collection<Runnable> threadInitializers, 
            Map<String,Object> variables, 
            Reader schemeScript, File currentDirectory, File schemeScriptFile, String schemeScriptURI ) {
        return schemeSecretary.executeSecretarially( new SecretaryMessage.NoThrow<Scheme,SchemeUtils.ExecuteSchemeResult>() {
            @Override
            public SchemeUtils.ExecuteSchemeResult execute0(Scheme scheme, Object[] args) {
                return SchemeUtils.evaluateScheme(scheme, threadInitializers, variables, schemeScript, currentDirectory, schemeScriptFile, schemeScriptURI);
            }
        }, Invokable.NOARG );
    }
}
