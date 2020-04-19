package lamu.lib.scheme;

import java.io.File;
import java.io.Reader;
import java.lang.invoke.MethodHandles;
import java.util.Arrays;
import java.util.HashMap;
import java.util.logging.Level;

import gnu.mapping.Procedure;
import lamu.lib.app.ApplicationComponent;
import lamu.lib.log.Logger;
import lamu.lib.scheme.proc.MultipleNamedProcedure1;
import lamu.lib.scheme.proc.MultipleNamedProcedure2;

public class SchemeEngine implements Evaluator, ApplicationComponent {
    public static SchemeEngine createLocalEngine() {
        SchemeEngine engine = new SchemeEngine();
        engine.getEvaluatorManager().addEvaluatorList( Arrays.asList( new SchemeEvaluator() ));
        return engine;
    }
    public static SchemeEngine createEmpty() {
        return new SchemeEngine();
    }
    private SchemeEngine() {
    }

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
    
    private final EvaluatorManager evaluatorManager = new EvaluatorManager();
    public EvaluatorManager getEvaluatorManager() {
        return evaluatorManager;
    }
    
    private final ThreadManager threadManager = new ThreadManager();
    public ThreadManager getThreadManager() {
        return threadManager;
    }

    //////////////////////////////////////////////////////////////////////////////////////////
    //
    //////////////////////////////////////////////////////////////////////////////////////////


    private static final ThreadLocal<SchemeEngine> threadLocal = new ThreadLocal<>();
    public static SchemeEngine getCurrent() {
        return threadLocal.get();
    }
    public static void setCurrent( SchemeEngine engine ) {
        threadLocal.set( engine );
    }
    public static boolean isPresent() {
        return threadLocal.get() != null;
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
        initializeEvaluator();
    }
    @Override
    public void processQuit() {
        finalizeEvaluator();
    }
    @Override
    public void initializeEvaluator() {
        this.getEvaluatorManager().initialize();
    }
    @Override
    public void finalizeEvaluator() {
        this.getEvaluatorManager().finalize();
    }

    @Override
    public SchemeResult evaluate(
        Runnable threadInitializer, 
        Reader schemeScript, 
        File currentDirectory,
        File currentFile, 
        String currentURI) 
    {
        return this.getEvaluatorManager().getCurrentEvaluator().evaluate(
            threadInitializer, 
            schemeScript, 
            currentDirectory, 
            currentFile, 
            currentURI );
    }

    @Override
    public void evaluate(
        Runnable threadInitializer, 
        String schemeScript,
        EvaluatorReceiver receiver,
        File currentDirectory, 
        File currentFile, 
        String currentURI )
    {
        Evaluator evaluator = this.getEvaluatorManager().getCurrentEvaluator();
        
        this.getThreadManager().startThread(
            Evaluator.createEvaluationRunner(
                threadInitializer, 
                schemeScript, 
                evaluator, 
                receiver, 
                currentDirectory, 
                currentFile, 
                currentURI ));
    }
}
