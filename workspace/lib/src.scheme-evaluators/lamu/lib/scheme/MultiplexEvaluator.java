package lamu.lib.scheme;

import java.io.File;
import java.io.Reader;
import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.WeakHashMap;
import java.util.logging.Level;

import gnu.mapping.Procedure;
import lamu.lib.app.ApplicationComponent;
import lamu.lib.log.Logger;
import lamu.lib.scheme.proc.MultipleNamedProcedure1;
import lamu.lib.scheme.proc.MultipleNamedProcedure2;

public class MultiplexEvaluator implements Evaluator, ApplicationComponent {
    public static interface MultipleEvaluatorListener {
        void notifyUpdate( MultiplexEvaluator multiplexEvaluator );
    }
    
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }

    public static MultiplexEvaluator createLocal() {
        MultiplexEvaluator engine = new MultiplexEvaluator();
        engine.addAllEvaluators( Arrays.asList( new SchemeEvaluator() ));
        return engine;
    }
    public static MultiplexEvaluator createEmpty() {
        return new MultiplexEvaluator();
    }
    private MultiplexEvaluator() {
    }

    //////////////////////////////////////////////////////////////////////////////////////////
    //
    //////////////////////////////////////////////////////////////////////////////////////////

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

    //////////////////////////////////////////////////////////////////////////////////////////
    //
    //////////////////////////////////////////////////////////////////////////////////////////
    
    private final ThreadManager threadManager = new ThreadManager();
    public ThreadManager getThreadManager() {
        return threadManager;
    }

    //////////////////////////////////////////////////////////////////////////////////////////
    //
    //////////////////////////////////////////////////////////////////////////////////////////

    private static final ThreadLocal<MultiplexEvaluator> threadLocal = new ThreadLocal<>();
    public static MultiplexEvaluator getCurrent() {
        return threadLocal.get();
    }
    public static void setCurrent( MultiplexEvaluator engine ) {
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
    //////////////////////////////////////////////////////////////////////////////////////////
    //
    //////////////////////////////////////////////////////////////////////////////////////////
    private Evaluator currentEvaluator;
    public Evaluator getCurrentEvaluator() {
        return currentEvaluator;
    }
    public void setCurrentEvaluator( Evaluator currentEvaluator ) {
        this.currentEvaluator = currentEvaluator;
        notifyUpdate();
    }
    private List<Evaluator> evaluatorList = new ArrayList<>();
    public List<Evaluator> getEvaluatorList() {
        return Collections.unmodifiableList( this.evaluatorList );
    }
    public void addEvaluator( Evaluator evaluator ) {
        if ( evaluator == null )
            throw new NullPointerException();
        this.evaluatorList.add( evaluator );
        if ( this.currentEvaluator == null )
            this.currentEvaluator = evaluator;
        this.notifyUpdate();
    }
    public void addAllEvaluators( Collection<Evaluator> evaluatorList ) {
        if ( ! evaluatorList.isEmpty() ) {
            this.evaluatorList.addAll( evaluatorList );
            if ( this.currentEvaluator == null )
                this.currentEvaluator = evaluatorList.iterator().next();
            this.notifyUpdate();
        }
    }
    @Override
    public void initializeEvaluator() {
        logInfo( "EvaluatorManager.initialize()" );
        for ( Evaluator e :  this.evaluatorList ) {
            e.initializeEvaluator();
        }
    }
    @Override
    public void finalizeEvaluator() {
        for ( Evaluator e :  this.evaluatorList ) {
            e.finalizeEvaluator();
        }
    }

    private long listenerCounter=0;
    private final Map<Long,MultipleEvaluatorListener> evaluatorListenerList = new WeakHashMap<>();
    public void addListener( MultipleEvaluatorListener listener ) {
        this.evaluatorListenerList.put( ++listenerCounter, listener );
    }
    public void removeListener( MultipleEvaluatorListener listener ) {
        for( Iterator<MultipleEvaluatorListener> i = this.evaluatorListenerList.values().iterator(); i.hasNext(); ) {
            MultipleEvaluatorListener currentListener = i.next();
            if ( listener == currentListener ) {
                i.remove();
            }
        }
    }
    
    public void notifyUpdate() {
        for ( MultipleEvaluatorListener l : evaluatorListenerList.values() ) {
            try {
                l.notifyUpdate(this);
            } catch ( Throwable t ){
                logError("", t);
            }
        }
    }

    //////////////////////////////////////////////////////////////////////////////////////////
    //
    //////////////////////////////////////////////////////////////////////////////////////////
    
    @Override
    public SchemeResult evaluate(
        Runnable threadInitializer, 
        Reader schemeScript, 
        File currentDirectory,
        File currentFile, 
        String currentURI) 
    {
        return this.getCurrentEvaluator().evaluate(
            threadInitializer, 
            schemeScript, 
            currentDirectory, 
            currentFile, 
            currentURI );
    }

    @Override
    public void evaluateAsync(
        Runnable threadInitializer, 
        String schemeScript,
        EvaluatorReceiver receiver,
        File currentDirectory, 
        File currentFile, 
        String currentURI )
    {
        Evaluator evaluator = this.getCurrentEvaluator();
        
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
