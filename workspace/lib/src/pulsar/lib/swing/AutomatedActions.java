package pulsar.lib.swing;

import java.lang.invoke.MethodHandles;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;

import javax.swing.Action;

import pulsar.lib.log.PulsarLogger;

public class AutomatedActions {
    static final PulsarLogger LOGGER = PulsarLogger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }

    public static List<Action> getActionList(Object o) {
        List<Action> actionList= new ArrayList<>();
        for ( Field f : o.getClass().getFields() ) {
            AutomatedActionField annotation = f.getAnnotation( AutomatedActionField.class );
            if ( annotation != null ) {
                try {
                    actionList.add( (Action) f.get( o ) );
                } catch (IllegalArgumentException | IllegalAccessException e) {
                    logError( "Ignored an exception", e );
                }
            }
        }
        return actionList;
    }
}
