package pulsar.lib.app.args;

import java.lang.invoke.MethodHandles;
import java.lang.management.ManagementFactory;
import java.lang.management.RuntimeMXBean;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Deque;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;

import pulsar.lib.app.ApplicationComponent;
import pulsar.lib.app.ApplicationVessel;
import pulsar.lib.log.PulsarLogger;
import pulsar.lib.thread.ThreadInitializer;
import pulsar.lib.thread.ThreadInitializerCollection;
import pulsar.lib.thread.ThreadInitializerCollectionContainer;
import pulsar.lib.thread.ThreadInitializerContainer;

public abstract class ArgumentParserDefault implements ArgumentParser {
    static final PulsarLogger LOGGER = PulsarLogger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }

    public static final ArgumentParserStackKey<Runnable> RUNNABLE  = new ArgumentParserStackKey<>();

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
                    System.err.println( "invoke:"+ r  );
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
    

    private ArrayList<ApplicationVessel> applicationVesselList = new ArrayList<>();
    public List<ApplicationVessel> getApplicationVesselList() {
        return Collections.unmodifiableList( applicationVesselList );
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
        createValueStackMap();
    }
    protected void clearValueStackMap() {
        stackMap.clear();
        stackList.clear();
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
        System.err.println( "getBootClassPath:"+runtimeMxBean.getBootClassPath() );
        System.err.println( "getClassPath:"+runtimeMxBean.getClassPath() );
    }

    ArgumentParserElement defaultArgumentParserElement = getFactoryMap().get( "default" ).create();
    ArgumentParserElement currentArgumentParserElement = defaultArgumentParserElement;
    

    private static void addInitializerContainer( Collection<ThreadInitializer> destination, Collection source ) {
        for ( Object o : source ) {
            if ( o instanceof ThreadInitializerContainer ) {
                destination.add(((ThreadInitializerContainer)o).getThreadInitializer());
                // Add the only first one.  (Fri, 20 Dec 2019 05:01:24 +0900)
                break;
            }
        }
    }
    private static void addInitializerCollectionContainer( Collection<ThreadInitializerCollection> destination, Collection source ) {
        for ( Object o : source ) {
            if ( o instanceof ThreadInitializerCollectionContainer ) {
                destination.add(((ThreadInitializerCollectionContainer)o).getThreadInitializerCollection());
            }
        }
    }

    void deploy() {
        // Collect thread initializers and set to collections.
        ArrayList<ThreadInitializer> threadInitializerList = new ArrayList<>(); 
        ArrayList<ThreadInitializerCollection> threadInitializerCollectionList = new ArrayList<>(); 
        {
            // Collect all thread initializers.
            for ( Deque stack : this.stackList ) {
                addInitializerContainer( threadInitializerList, stack );
            }
            // Collect all thread initializer collections.
            for ( Deque stack : this.stackList ) {
                addInitializerCollectionContainer( threadInitializerCollectionList, stack );
            }
            // then, add the initializers to the collections.
            for ( ThreadInitializerCollection c : threadInitializerCollectionList ) {
                c.addAllThreadInitializer( threadInitializerList );
            }
        }
        
        ApplicationVessel vessel = new ApplicationVessel();
        
        vessel.getThreadInitializerCollection().addAllThreadInitializer( threadInitializerList );
        for ( Deque stack : stackList ) {
            for ( Object o : stack ) {
                if ( o instanceof ApplicationComponent ) {
                    vessel.add((ApplicationComponent) o );
                }
            }
        }
        
//        vessel.addAll( getValueStack( SCHEME_ENGINE ) );
//        vessel.addAll( getValueStack( PULSAR ) );
//        vessel.addAll( getValueStack( KAWAPAD ) );
//        vessel.addAll( getValueStack( FRAME ) );
//        vessel.addAll( getValueStack( SCHEME_HTTP ) );
        
        // Executing runnable stack;
        {
            ArrayList<Runnable> list = new ArrayList<Runnable>( this.getValueStack( RUNNABLE ) );
            Collections.reverse( list );
            vessel.add( new RunnableInitializer( list ) );
        }
 
        applicationVesselList.add( vessel );

//        vessel.getThreadInitializerCollection().initialize();
//        
//        for ( ThreadInitializerCollection c : threadInitializerCollectionList ) {
//            logInfo( "deploy : "+ c.id + ":" + c.toString() );
//        }
//        vessel.processInit();

        //

        initValueStackState();
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
    
    public void parse( List<String> args ) {
        for ( int i=0; i<args.size(); i++ ) {
            String s = args.get( i );
            logInfo( "args[" + i +"]=\"" + s  + "\"");
            notifyArg( s );
        }
        notifyEnd();
        notifyAllEnd();
    }


}
