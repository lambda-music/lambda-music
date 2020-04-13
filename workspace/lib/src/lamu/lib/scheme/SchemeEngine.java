package lamu.lib.scheme;

import java.io.File;
import java.lang.invoke.MethodHandles;
import java.util.HashMap;
import java.util.logging.Level;

import gnu.mapping.Procedure;
import lamu.lib.CurrentObject;
import lamu.lib.app.ApplicationComponent;
import lamu.lib.log.Logger;
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
    }
    
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
}
