package lamu.lib.scheme;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.logging.Level;

import lamu.lib.log.Logger;


public class EvaluatorManager {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }
    public EvaluatorManager() {
    }
    public static void initEvaluatorManager( EvaluatorManager manager, List<String> urls ) {
        ArrayList<Evaluator> list = new ArrayList<>();
        for ( String url : urls ) {
            list.add( new RemoteEvaluator( url ) );
        }
        manager.getEvaluatorList().addAll( list );
    }

    private Evaluator currentEvaluator;
    public Evaluator getCurrentEvaluator() {
        return currentEvaluator;
    }
    public void setCurrentEvaluator(Evaluator currentEvaluator) {
        this.currentEvaluator = currentEvaluator;
    }
    private List<Evaluator> evaluatorList = new ArrayList<>();
    public List<Evaluator> getEvaluatorList() {
        return this.evaluatorList;
    }
    public void addEvaluatorList( Collection<Evaluator> evaluatorList ) {
        if ( ! evaluatorList.isEmpty() ) {
            this.getEvaluatorList().addAll( evaluatorList );
            if ( this.getCurrentEvaluator() == null )
                this.setCurrentEvaluator( evaluatorList.iterator().next() );
        }
    }
    public void initialize() {
        logInfo( "EvaluatorManager.initialize()" );
        for ( Evaluator e :  this.evaluatorList ) {
            e.initializeEvaluator();
        }
    }
    public void finalize() {
        for ( Evaluator e :  this.evaluatorList ) {
            e.finalizeEvaluator();
        }
    }
}
