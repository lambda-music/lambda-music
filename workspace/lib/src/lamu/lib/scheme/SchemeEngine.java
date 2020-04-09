package lamu.lib.scheme;

import java.io.File;
import java.lang.invoke.MethodHandles;
import java.util.HashMap;
import java.util.logging.Level;

import gnu.mapping.Environment;
import gnu.mapping.Procedure;
import kawa.standard.Scheme;
import lamu.lib.CurrentObject;
import lamu.lib.app.ApplicationComponent;
import lamu.lib.log.Logger;
import lamu.lib.scheme.SchemeEvaluator.SchemeEngineListener;
import lamu.lib.scheme.proc.MultipleNamedProcedure0;
import lamu.lib.scheme.proc.MultipleNamedProcedure1;
import lamu.lib.scheme.proc.MultipleNamedProcedure2;
import lamu.lib.thread.ThreadInitializer;
import lamu.lib.thread.ThreadInitializerContainer;

public class SchemeEngine implements ThreadInitializerContainer<SchemeEngine>, ApplicationComponent {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }

    static HashMap<Object,Object> map = new HashMap<>(); 
    public static final Procedure putvar = new MultipleNamedProcedure2() {
        @Override
        public Object apply2(Object arg1, Object arg2) throws Throwable {
            map.put( arg1, arg2 );
            return SchemeUtils.NO_RESULT;
        }
    };
    public static final Procedure getvar = new MultipleNamedProcedure1() {
        @Override
        public Object apply1(Object arg1) throws Throwable {
            Object result = map.get( arg1 );
            if ( result == null ) {
                return false;
            } else {
                return result;
            }
        }
    };

    ///////////////////////////////////////////////////////////////////////////////////////////////////
    
    private final EvaluatorManager<SchemeEvaluator> evaluatorManager = new EvaluatorManager<>( new SchemeEvaluator() );
    public EvaluatorManager<SchemeEvaluator> getEvaluatorManager() {
        return evaluatorManager;
    }
    
    private final ThreadManager threadManager = new ThreadManager();
    public ThreadManager getThreadManager() {
        return threadManager;
    }

    //////////////////////////////////////////////////////////////////////////////////////////
    //
    //////////////////////////////////////////////////////////////////////////////////////////

    public SchemeEngine() {
        SchemeEngine.registerSchemeInitializer( this );
    }
    
    //////////////////////////////////////////////////////////////////////////////////////////
    //
    //////////////////////////////////////////////////////////////////////////////////////////
    
    private static final CurrentObject<SchemeEngine> currentObject = new CurrentObject<>( SchemeEngine.class );
    private final ThreadInitializer<SchemeEngine> threadInitializer =
            ThreadInitializer.createMultipleThreadInitializer( "scheme-engine", this,
                ThreadInitializer.createThreadInitializer( "current-scheme-engine", currentObject, this ),
                this.getEvaluatorManager().getPrimaryEvaluator().getThreadInitializer()
                );
    
    @Override
    public ThreadInitializer<SchemeEngine> getThreadInitializer() {
        return threadInitializer;
    }
    public static SchemeEngine getCurrent() {
        return currentObject.get();
    }
    public static boolean isPresent() {
        return currentObject.isPresent();
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
        this.getEvaluatorManager().getPrimaryEvaluator().processInit();
        this.getEvaluatorManager().initialize();
    }
    @Override
    public void processQuit() {
        this.getEvaluatorManager().getPrimaryEvaluator().processQuit();
        this.getEvaluatorManager().finalize();
    }
    
    public static Runnable createEvaluationRunner(
            Runnable threadInitializer, 
            String schemeScript,
            Evaluator evaluator,
            EvaluatorReceiver receiver, 
            File currentDirectory, 
            File currentFile, 
            String currentURI )
    {
        return new EvaluatorRunnable( 
            threadInitializer, 
            schemeScript, 
            evaluator, 
            receiver, 
            currentDirectory,
            currentFile, 
            currentURI );
    }
    
    public static void registerSchemeInitializer( SchemeEngine engine ) {
        engine.getEvaluatorManager().getPrimaryEvaluator().registerSchemeInitializer( initSchemeListener );
    }
    private static SchemeEngineListener initSchemeListener = new SchemeEngineListener() {
        @Override
        public void execute(Scheme scheme) {
            initScheme( scheme );
        }
    };
    static void initScheme( Scheme scheme ) {
        Environment env = scheme.getEnvironment();
        SchemeUtils.defineLambda(env, new MultipleNamedProcedure0( "scheme-engine" ) {
            @Override
            public Object apply0() throws Throwable {
                return SchemeEngine.getCurrent();
            }
        }  );
        SchemeUtils.defineLambda(env, new MultipleNamedProcedure0( "scheme-engine-present?" ) {
            @Override
            public Object apply0() throws Throwable {
                return SchemeEngine.isPresent();
            }
        } );

//        Environment env = scheme.getEnvironment();
//        SchemeUtils.defineVar(env, new Procedure0() {
//            @Override
//            public Object apply0() throws Throwable {
//                getCurrent().getSchemeEvaluator().newScheme();
//                return true;
//            }
//        }, "reset-scheme" );
    }
}
