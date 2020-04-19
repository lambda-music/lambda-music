package lamu.lib.app.args;

import java.lang.invoke.MethodHandles;
import java.lang.management.ManagementFactory;
import java.lang.management.RuntimeMXBean;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Deque;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;

import lamu.lib.app.ApplicationComponent;
import lamu.lib.app.ApplicationVessel;
import lamu.lib.log.Logger;
import lamu.lib.stream.Stream;

public abstract class ArgumentParserDefault implements ArgumentParser {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }

    public static final ArgumentParserStackKey<Runnable> RUNNABLE_INIT  = new ArgumentParserStackKey<>();
    public static final ArgumentParserStackKey<Runnable> RUNNABLE_START = new ArgumentParserStackKey<>();
    public static final ArgumentParserStackKey<ApplicationVessel> VESSELS = new ArgumentParserStackKey<>();
    public static final ArgumentParserStackKey<Stream> STREAMABLES = new ArgumentParserStackKey<>();

    static final class DefaultArgumentParserElementFactory implements ArgumentParserElementFactory {
        @Override
        public
        ArgumentParserElement create() {
            return new ArgumentParserElement() {
                @Override
                public ArgumentParserElement notifyArg(ArgumentParser parser, String s) {
                    ArgumentParserElementFactory f = parser.getFactory( s );
                    if ( f == null ) {
                        throw new RuntimeException( "unknown command \"" + s + "\"" );
                    } else {
                        return f.create(); 
                    }
                }
                @Override
                public void notifyEnd(ArgumentParser parser) {
                }
            };
        }
    }
    static class RunnableInitializer implements ApplicationComponent {
        private ApplicationComponent parent;
        @Override
        public void setParentApplicationComponent(ApplicationComponent parent) {
            this.parent = parent;
        }

        @Override
        public ApplicationComponent getParentApplicationComponent() {
            return parent;
        }
        
        final List<Runnable> runnableStack = new ArrayList<>();
        public RunnableInitializer( List<Runnable> runnableStack ) {
            this.runnableStack.addAll( runnableStack );
        }

        @Override
        public void processInit() {
            for ( Runnable r : runnableStack ) {
                try {
                    logInfo( "invoke:"+ r  );
                    r.run();
                } catch (Exception e) {
                    logError( "", e );
                }
            }
        }
        @Override
        public void processQuit() {
        }
    }
    

    private ArrayList<Deque> stackList = new ArrayList<>();
    private Map<ArgumentParserStackKey,ArrayDeque> stackMap = new LinkedHashMap<>();
    @Override
    public <T> Deque<T> getValueStack( ArgumentParserStackKey<T> key ) {
        if ( ! stackMap.containsKey( key ) ) {
            ArrayDeque<Object> d = new ArrayDeque<>();
            stackMap.put( key, d );
            stackList.add( d );
        }
        return stackMap.get( key );
    }

    protected void initValueStackState() {
        clearValueStackMap();
        createDefaultValueStackMap();
        createValueStackMap();
    }
    private void clearValueStackMap() {
        stackMap.clear();
        stackList.clear();
    }
    private void createDefaultValueStackMap() {
        getValueStack( VESSELS );
        getValueStack( STREAMABLES );
        getValueStack( RUNNABLE_INIT );
    }
    
    protected abstract void createValueStackMap();

    private HashMap<String,ArgumentParserElementFactory> factoryMap = new HashMap<>();
    public Map<String,ArgumentParserElementFactory> getFactoryMap() {
        return factoryMap;
    }
    
    public void registerFactory( String key,  ArgumentParserElementFactory value ) {
        factoryMap.put( key, value );
    }
    public ArgumentParserElementFactory getFactory( String key ) {
        return factoryMap.get( key );
    }

    {
        initValueStackState();
        registerFactory( "default", new DefaultArgumentParserElementFactory());
    }
    {
        
        RuntimeMXBean runtimeMxBean = ManagementFactory.getRuntimeMXBean();
        List<String> arguments = runtimeMxBean.getInputArguments();
        
        System.err.println( arguments );
        try {
            System.err.println( "getBootClassPath:"+runtimeMxBean.getBootClassPath() );
        } catch ( Throwable e ) {
            e.printStackTrace();
        }
        try {
            System.err.println( "getClassPath:"+runtimeMxBean.getClassPath() );
        } catch ( Throwable e ) {
            e.printStackTrace();
        }
    }

    ArgumentParserElement defaultArgumentParserElement = getFactoryMap().get( "default" ).create();
    ArgumentParserElement currentArgumentParserElement = defaultArgumentParserElement;
    
    void deploy() {
        // Collect all components
        ArrayList<Object> allObjects = new ArrayList<>();
        ArrayList<ApplicationComponent> allComponents = new ArrayList<>();

        {
            ArrayList<Deque> stackList2 = new ArrayList<>( this.stackList );
            
            // The vessels are not necessary here; therefore exclude it from the list.
            stackList2.remove( this.getValueStack( VESSELS ) );
            stackList2.remove( this.getValueStack( STREAMABLES ) );
            
            logInfo( "deploy:==== COLLECT COMPONENTS ==============================" );
            for ( Deque stack : stackList2 ) {
                for ( Object o : stack ) {
                    logInfo( "deploy:"  + o.toString() );
                    
                    // 1.
                    allObjects.add(o);
                    
                    // 2.
                    if ( o instanceof ApplicationComponent ) {
                        allComponents.add((ApplicationComponent) o );
                    }
                }
            }
            logInfo( "deploy:======================================================" );
        }

        // Initialize ThreadInitializer
        ApplicationVessel vessel = new ApplicationVessel( "ExecVessel" );

        // Collect all application compnents.
        vessel.addAll( allComponents );
        
        // Executing runnable(init) stack;
        {
            ArrayList<Runnable> list = new ArrayList<Runnable>( this.getValueStack( RUNNABLE_INIT ) );
            Collections.reverse( list );
            vessel.add( new RunnableInitializer( list ) );
        }

        // Executing runnable(start) stack;
        {
            ArrayList<Runnable> list = new ArrayList<Runnable>( this.getValueStack( RUNNABLE_START ) );
            Collections.reverse( list );
            vessel.add( new RunnableInitializer( list ) );
        }
 
        this.getValueStack( VESSELS ).push( vessel );
    }
    private void notifyArg(String s) {
        ArgumentParserElement nextArgumentParserElement;
        if ( "+".equals( s ) ) {
            nextArgumentParserElement = defaultArgumentParserElement;
        } else {
            nextArgumentParserElement = currentArgumentParserElement.notifyArg( this, s );
        }
        if ( nextArgumentParserElement != currentArgumentParserElement ) {
            notifyEnd();
        }
        currentArgumentParserElement=nextArgumentParserElement;
    }

    private void notifyEnd() {
        if ( currentArgumentParserElement != null ) {
            // If currentElement is default element, notifyEnd() does not do anything;
            // otherwise calling notifyEnd() does its clean up process.
            // (Thu, 05 Dec 2019 15:32:18 +0900)
            currentArgumentParserElement.notifyEnd( this );
        }
    }

    private void notifyAllEnd() {
        deploy();
    }
    
    public void parse( List<String> arguments ) {
        for ( int i=0; i<arguments.size(); i++ ) {
            String s = arguments.get( i );
            logInfo( "args[" + i +"]=\"" + s  + "\"");
            notifyArg( s );
        }
        notifyEnd();
        notifyAllEnd();
    }


}
