package pulsar.lib.scheme;

import java.lang.invoke.MethodHandles;
import java.util.HashMap;
import java.util.logging.Level;
import java.util.logging.Logger;

import gnu.mapping.Procedure;
import gnu.mapping.Procedure1;
import gnu.mapping.Procedure2;
import pulsar.lib.CurrentObject;
import pulsar.lib.app.ApplicationComponent;
import pulsar.lib.thread.ThreadInitializer;
import pulsar.lib.thread.ThreadInitializerContainer;

public class SchemeEngine implements ThreadInitializerContainer<SchemeEngine>, ApplicationComponent {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }

    static HashMap<Object,Object> map = new HashMap<>(); 
    public static final Procedure putvar = new Procedure2() {
        @Override
        public Object apply2(Object arg1, Object arg2) throws Throwable {
            map.put( arg1, arg2 );
            return SchemeUtils.NO_RESULT;
        }
    };
    public static final Procedure getvar = new Procedure1() {
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
    
    private final SchemeEvaluator schemeEvaluator = new SchemeEvaluator();
    public SchemeEvaluator getSchemeEvaluator() {
        return schemeEvaluator;
    }
    
    private final EvaluatorManager evaluatorManager = new EvaluatorManager( this.schemeEvaluator );
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

    public SchemeEngine() {
    }
    
    //////////////////////////////////////////////////////////////////////////////////////////
    //
    //////////////////////////////////////////////////////////////////////////////////////////
    
    private static final CurrentObject<SchemeEngine> currentObject = new CurrentObject<>( SchemeEngine.class );
    private final ThreadInitializer<SchemeEngine> threadInitializer =
            ThreadInitializer.createMultipleThreadInitializer( "scheme-engine", this,
                ThreadInitializer.createThreadInitializer( "current-scheme-engine", currentObject, this ),
                this.getSchemeEvaluator().getThreadInitializer()
                );
    
    @Override
    public ThreadInitializer<SchemeEngine> getThreadInitializer() {
        return threadInitializer;
    }
    public static SchemeEngine getCurrent() {
        return currentObject.get();
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
        this.getSchemeEvaluator().processInit();
    }
    @Override
    public void processQuit() {
        this.getSchemeEvaluator().processQuit();
    }
}
