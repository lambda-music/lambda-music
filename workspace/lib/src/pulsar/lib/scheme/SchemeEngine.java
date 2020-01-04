package pulsar.lib.scheme;

import java.lang.invoke.MethodHandles;
import java.util.logging.Level;
import java.util.logging.Logger;

import pulsar.lib.CurrentObject;
import pulsar.lib.app.ApplicationComponent;
import pulsar.lib.thread.ThreadInitializer;
import pulsar.lib.thread.ThreadInitializerContainer;

public class SchemeEngine implements ThreadInitializerContainer<SchemeEngine>, ApplicationComponent {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }

    ///////////////////////////////////////////////////////////////////////////////////////////////////
    
    private final SchemeEvaluator schemeEvaluator = new SchemeEvaluator();
    public SchemeEvaluator getSchemeEvaluator() {
        return schemeEvaluator;
    }
    
    private final EvaluatorList evaluatorList = new EvaluatorList();
    public EvaluatorList getEvaluatorList() {
        return evaluatorList;
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
