package pulsar.lib.scheme;

import java.util.Arrays;
import java.util.List;

import javax.swing.Action;
import javax.swing.KeyStroke;

import pulsar.lib.swing.AcceleratorKeyList;
import pulsar.lib.swing.Action2;
import pulsar.lib.swing.AutomatedActions;

public class DescriptiveActions {
    public static final String formatActions( Object o ) {
        StringBuilder sb = new StringBuilder();
        sb.append( "|" );
        sb.append( " ID " );
        sb.append( "|" );
        sb.append( " Description " );
        sb.append( "|" );
        sb.append( " Keybind " );
        sb.append( "|" );
        sb.append( "\n" );
        sb.append( "|" );
        sb.append( " :----: " );
        sb.append( "|" );
        sb.append( " :----: " );
        sb.append( "|" );
        sb.append( " :----- " );
        sb.append( "|" );
        sb.append( "\n" );

        List<Action> actionList = AutomatedActions.getActionList( o );
        for ( Action action : actionList ) {
            String name = (String) action.getValue( Action.NAME );
            String caption = (String) action.getValue( Action2.CAPTION );
            if ( caption == null ) {
                caption = name;
            }
            if ( caption == null ) {
                throw new IllegalStateException();
            }
            List<KeyStroke> keyStrokeList = (List<KeyStroke>) action.getValue( AcceleratorKeyList.ACCELERATOR_KEY_LIST );
            if ( keyStrokeList == null ) {
                Object acceleratorKey = action.getValue( Action.ACCELERATOR_KEY );
                if ( acceleratorKey == null ) {
                    keyStrokeList =  (List)Arrays.asList();
                } else {
                    keyStrokeList =  (List)Arrays.asList( acceleratorKey );
                }
            }
            if ( keyStrokeList.isEmpty() )  {
                System.err.println( "WARNING :: NO ACCELERATOR KEY WAS FOUND " + action );
//                continue;
            }
            
            sb.append( "|" );
            sb.append( name );
            sb.append( "|" );
            sb.append( caption );
            sb.append( "|" );
            
            {
                boolean flg =false;
                sb.append( "  " );
                for ( KeyStroke keyStroke : keyStrokeList ) {
                    if ( flg ) {
                        sb.append( ", " );
                    } else {
                        flg = true;
                    }
                    String s = keyStroke.toString();
                    s= s.replaceAll( "pressed|released", "" );
                    s= s.trim();
                    s= s.replaceAll( " +", "-" );
                    s= s.toUpperCase();
                    sb.append( s );
                }
                sb.append( "  " );
            }
            sb.append( "|" );
            sb.append( "\n" );
        }
        return sb.toString();
    }
}
