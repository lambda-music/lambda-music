package pulsar.lib.swing;

import java.lang.invoke.MethodHandles;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.Action;
import javax.swing.text.JTextComponent;

public class AutomatedActions {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }

    public static List<Action> getActionList(JTextComponent c) {
        List<Action> actionList= new ArrayList<>();
        for ( Field f : c.getClass().getFields() ) {
            AutomatedActionField annotation = f.getAnnotation( AutomatedActionField.class );
            if ( annotation != null ) {
                try {
                    actionList.add( (Action) f.get( c ) );
                } catch (IllegalArgumentException | IllegalAccessException e) {
                    logError( "Ignored an exception", e );
                }
            }
        }
        return actionList;
    }
}
