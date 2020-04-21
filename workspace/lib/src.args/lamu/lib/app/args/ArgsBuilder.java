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
import lamu.lib.log.Logger;

public class ArgsBuilder {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }

    public ArgsBuilder() {
    }
    
    public static class Initializer implements ApplicationComponent {
        private ApplicationComponent parent;
        @Override
        public void setParentApplicationComponent(ApplicationComponent parent) {
            this.parent = parent;
        }

        @Override
        public ApplicationComponent getParentApplicationComponent() {
            return parent;
        }
        
        final List<Runnable> runnableList = new ArrayList<>();
        public Initializer( List<Runnable> runnableList ) {
            this.runnableList.addAll( runnableList );
        }

        @Override
        public void processInit() {
            for ( Runnable r : runnableList ) {
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

    static final class DefaultArgumentParserElementFactory implements ArgsBuilderElementFactory {
        @Override
        public
        ArgsBuilderElement create() {
            return new ArgsBuilderElement() {
                @Override
                public ArgsBuilderElement notifyArg(ArgsBuilder parser, String s) {
                    ArgsBuilderElementFactory f = parser.getFactory( s );
                    if ( f == null ) {
                        throw new RuntimeException( "unknown command \"" + s + "\"" );
                    } else {
                        return f.create(); 
                    }
                }
                @Override
                public void notifyEnd(ArgsBuilder parser) {
                }
            };
        }
    }
    

    private ArrayList<Deque> valueStackList = new ArrayList<>();
    public List<Deque> getValueStackList() {
        return Collections.unmodifiableList( this.valueStackList );
    }
    
    private Map<ArgsBuilderStackKey,ArrayDeque> valueStackMap = new LinkedHashMap<>();
    public <T> Deque<T> getValueStack( ArgsBuilderStackKey<T> key ) {
        if ( ! valueStackMap.containsKey( key ) ) {
            ArrayDeque<Object> d = new ArrayDeque<>();
            valueStackMap.put( key, d );
            valueStackList.add( d );
        }
        return valueStackMap.get( key );
    }

    public void clear() {
        this.valueStackMap.clear();
        this.valueStackList.clear();
    }
    
    private HashMap<String,ArgsBuilderElementFactory> factoryMap = new HashMap<>();
    public Map<String,ArgsBuilderElementFactory> getFactoryMap() {
        return factoryMap;
    }
    
    public void registerFactory( String key,  ArgsBuilderElementFactory value ) {
        factoryMap.put( key, value );
    }
    public ArgsBuilderElementFactory getFactory( String key ) {
        return factoryMap.get( key );
    }

    {
//        initValueStackState();
        registerFactory( "default", new DefaultArgumentParserElementFactory() );
    }
    
    static void demonstrateRuntime() {
        
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

    ArgsBuilderElement defaultArgumentParserElement = getFactoryMap().get( "default" ).create();
    ArgsBuilderElement currentArgumentParserElement = defaultArgumentParserElement;
    
    
    private void notifyArg(String s) {
        ArgsBuilderElement nextArgumentParserElement;
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
