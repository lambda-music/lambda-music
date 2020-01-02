package kawapad;

import java.lang.invoke.MethodHandles;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.logging.Level;
import java.util.logging.Logger;

import gnu.expr.Language;
import gnu.mapping.Environment;
import gnu.mapping.Procedure;
import gnu.mapping.Symbol;
import kawa.standard.Scheme;

public class KawapadEventHandlers {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }
    static final class SchemeProcedure {
        /*
         *  The variable environment and language are not necessary anymore
         *  but there are many that setting these so left them their be.
         *  This class should also be replaced to Invokable.
         *  (Wed, 24 Jul 2019 16:21:59 +0900)
         */
        @SuppressWarnings("unused")
        private final Environment environment;
        @SuppressWarnings("unused")
        private final Language language;
        private final Procedure procedure;
        SchemeProcedure( Procedure procedure , Environment environmen ) {
            this.environment = environmen;
            this.language = Language.getDefaultLanguage();
            this.procedure = procedure;
        }
        public Object invoke( Object... args ) {
            try {
//                  Environment.setCurrent( this.environment );
//                  Environment.setCurrent( environment );
//                  Language.setCurrentLanguage( this.language );
                return procedure.applyN( args );
            } catch (Throwable e) {
                logError( "SchemeInvokableProcedure:error" , e );
                return e;
                //                  throw new RuntimeException(e);
            }
        }
    }
    
    public static final String INIT      = "init";   // occurs when initializing scheme objects. (Tue, 06 Aug 2019 08:37:12 +0900)
    public static final String CREATE    = "create"; // occurs when creating form objects.       (Tue, 06 Aug 2019 08:37:12 +0900)
    public static final String CARET     = "caret";
    public static final String INSERT    = "insert";
    public static final String REMOVE    = "remove";
    public static final String ATTRIBUTE = "attribute";
    public static final String CHANGE    = "change";
    public static final String TYPED     = "typed";

    final Map<Symbol,Map<Symbol,SchemeProcedure>> map = new HashMap<>();
    {
        map.put( Symbol.valueOf(INIT),      new HashMap<>() );
        map.put( Symbol.valueOf(CREATE),    new HashMap<>() );
        map.put( Symbol.valueOf(CARET),     new HashMap<>() );
        map.put( Symbol.valueOf(INSERT),    new HashMap<>() );
        map.put( Symbol.valueOf(REMOVE),    new HashMap<>() );
        map.put( Symbol.valueOf(ATTRIBUTE), new HashMap<>() );
        map.put( Symbol.valueOf(CHANGE),    new HashMap<>() );
        map.put( Symbol.valueOf(TYPED),     new HashMap<>() );
    }
    Map<Symbol, SchemeProcedure> getEventType(Symbol eventTypeID) {
        Map<Symbol, SchemeProcedure> eventType = map.get( eventTypeID );
        if ( eventType == null )
            throw new RuntimeException( "unknown event type + ( " + eventTypeID + ")"  );
        return eventType;
    }
    public void clear() {
        Kawapad.logInfo("EventHandlers#clear()");
        map.get( Symbol.valueOf(INIT)).clear();
        map.get( Symbol.valueOf(CREATE)).clear();
        map.get( Symbol.valueOf(CARET)).clear();
        map.get( Symbol.valueOf(INSERT)).clear();
        map.get( Symbol.valueOf(REMOVE)).clear();
        map.get( Symbol.valueOf(ATTRIBUTE)).clear();
        map.get( Symbol.valueOf(CHANGE)).clear();
        map.get( Symbol.valueOf(TYPED)).clear();
    }
    Map<Symbol, SchemeProcedure> getEventType(String eventTypeID) {
        return getEventType( Symbol.valueOf(eventTypeID));
    }
    
    public void register( Symbol eventTypeID, Symbol procID, Procedure proc ) {
        register( eventTypeID, procID, new SchemeProcedure( proc, Environment.getCurrent() ) );
    }
    public void register( Symbol eventTypeID, Symbol procID, SchemeProcedure proc ) {
        Map<Symbol, SchemeProcedure> eventType = getEventType(eventTypeID);
        eventType.put( procID, proc );
    }
    public void unregister( Symbol eventTypeID, Symbol procID ) {
        Map<Symbol, SchemeProcedure> eventType = getEventType(eventTypeID);
        eventType.remove( procID );
    }
    public void invokeEventHandler( Kawapad kawapad, String eventTypeID, Object ... args ) {
//              logInfo( "eventHandlers.invokeEventHandler(outer)" );
        
        kawapad.getSchemeEngine().getThreadManager().startThread( new Runnable() {
            @Override
            public void run() {
                Scheme scheme = kawapad.getSchemeEngine().getScheme();
                kawapad.getThreadInitializerCollection().initialize();
                
                synchronized ( scheme ) {
                    //  logInfo( "eventHandlers.invokeEventHandler(inner)" );
//                            Environment env = scheme.getEnvironment();
                    for( Entry<Symbol,SchemeProcedure> e :  getEventType(eventTypeID).entrySet() ) {
                        try {
                            e.getValue().invoke( args );
                        } catch ( Throwable t ) {
                            Kawapad.logError("invoking event handlers : ", t);
                        }
                    }
                }
            }
        });
    }
    
//          void invokeEventHandlers( String )
    
}