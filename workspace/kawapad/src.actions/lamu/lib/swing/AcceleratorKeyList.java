package lamu.lib.swing;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;

import javax.swing.Action;
import javax.swing.ActionMap;
import javax.swing.InputMap;
import javax.swing.JComponent;
import javax.swing.KeyStroke;
import javax.swing.text.JTextComponent;

import lamu.lib.log.Logger;

public class AcceleratorKeyList {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }
    private static final boolean DEBUG = false;

    public static final String ACCELERATOR_KEY_LIST ="accelerator-key-list";
    public static void putAcceleratorKeyList( Action action, List<KeyStroke> keyStrokeList ) {
        if ( keyStrokeList == null || keyStrokeList.isEmpty() )
            return;
        action.putValue( Action.ACCELERATOR_KEY, keyStrokeList.get(0) );
        action.putValue( AcceleratorKeyList.ACCELERATOR_KEY_LIST, keyStrokeList );
    }
    public static void putAcceleratorKeyList( Action action, String ... keyStrokeStrings ) {
        putAcceleratorKeyList( action, getKeyStrokeList( keyStrokeStrings ) );
    }
    public static List<KeyStroke> getKeyStrokeList( String... keyStrokeStrings ) {
        ArrayList<KeyStroke> keyStrokeList = new ArrayList<>( keyStrokeStrings.length );
        for ( int i=0; i<keyStrokeStrings.length; i++ ) {
            keyStrokeList.add( getKeyStroke( keyStrokeStrings[i] ) );
        }
        return keyStrokeList;
    }

//    public void putAcceleratorKeyList( KeyStroke ... keyStrokes ) {
//        putAcceleratorKeyList( this, keyStrokes );
//    }
//    public <T> T init( JTextComponent c ) {
//        addAcceleratorKeyList( c, this );
//        return (T)this;
//    }
    
    public static KeyStroke getKeyStroke( String s ) {
        KeyStroke keyStroke = KeyStroke.getKeyStroke( s );
        if ( keyStroke == null )
            throw new IllegalArgumentException();
        return keyStroke;
    }
    public static KeyStroke getKeyStroke(int keyCode, int modifiers) {
        return KeyStroke.getKeyStroke( keyCode, modifiers );
    }
    
////////////////////////////////////////////////////////////////////////////////////////
    
    // This action intercepts our customization so delete it.
    public static void purgeKeyFromActionMap( ActionMap actionMap, Object key ) {
        actionMap.remove(key);
        if ( actionMap.getParent() != null )
            purgeKeyFromActionMap(actionMap.getParent(), key );
    }

    public static void purgeKeyFromActionMap( JComponent c, Object key ) {
        purgeKeyFromActionMap( c.getActionMap(), key );
    }

    public static void purgeActionFromActionMap( JComponent c, Action action ) {
        purgeKeyFromActionMap( c, action.getValue( Action.NAME ));
    }

    public static void addActionToActionMap( JComponent c, Action action ) {
        String name = (String)action.getValue( Action.NAME );
        c.getActionMap().put(name, action );
        if ( DEBUG )
            logInfo( name );
    }
    public static void addActionToInputMap( JComponent c, Action action ) {
        Object name = action.getValue( Action.NAME );
        List<KeyStroke> keyStrokes =  (List<KeyStroke>)action.getValue( AcceleratorKeyList.ACCELERATOR_KEY_LIST );
        if ( keyStrokes == null ) {
            KeyStroke acceleratorKey = (KeyStroke)action.getValue( Action.ACCELERATOR_KEY );
            if (acceleratorKey == null ) {
                // Ignore it.
                logWarn( "Could not initialize keystrokes for " + action.toString() );
            } else {
                logInfo( "addActionToInputMap1:" + name + "/" + acceleratorKey );
                c.getInputMap( JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT ).put(acceleratorKey, name );
            }
        } else {
            for ( KeyStroke keyStroke : keyStrokes ) {
                logInfo( "addActionToInputMap2:" + name + "/" + keyStroke );
                c.getInputMap( JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT ).put( keyStroke, name );
            }
        }
    }

    public static Action addAction( JComponent c, Action action ) {
        purgeActionFromActionMap( c, action );
        addActionToActionMap( c, action );
        addActionToInputMap( c, action );
        return action;
    }

    public static void addAcceleratorKey( JComponent component, Action action ) {
        Object name = action.getValue( Action.NAME );
        component.getInputMap().put( (KeyStroke) action.getValue( Action.ACCELERATOR_KEY ), name );
        component.getActionMap().put( name, action );
    }
    public static void addAcceleratorKeyList(JTextComponent c,Action action) {
        KeyStroke[] keyStrokes =  (KeyStroke[])action.getValue( AcceleratorKeyList.ACCELERATOR_KEY_LIST );
        for ( int i=1; i<keyStrokes.length; i++ ) {
            c.getKeymap().addActionForKeyStroke( keyStrokes[i], action );
        }
    }

    public static void dump(InputMap inputMap) {
        KeyStroke[] keys = inputMap.keys();
        for ( KeyStroke k : keys ) {
            System.err.print( k );
            System.err.print( '=' );
            System.err.print( inputMap.get( k ) );
            System.err.println( );
        }
    }

    public static void dump(ActionMap actionMap) {
        dumpProc(actionMap,0);
    }
    public static void dumpProc(ActionMap actionMap, int level ) {
        System.err.println( "dump: level-" + level );
        Object[] keys = actionMap.allKeys();
        for ( Object k : keys ) {
            Action obj = actionMap.get( k );
            System.err.print( k );
            System.err.print( '=' );
            System.err.print( obj.getValue(Action.NAME ) + ":" + obj.getValue(AcceleratorKeyList.ACCELERATOR_KEY_LIST) + "/" + obj.getValue(AcceleratorKeyList.ACCELERATOR_KEY_LIST) );
            System.err.println( );
        }
        System.err.println( "" );
        
        if ( actionMap.getParent() != null )
            dumpProc( actionMap.getParent() , level + 1);
    }
    
    public static void processAcceleratorKeys( JComponent o ) {
        List<Action> actionList = AutomatedActions.getActionList( o );
        for( Action action : actionList ) {
            addAction( o, action );
        }
    }

}
