package kawapad;

import java.awt.Dimension;
import java.awt.Point;
import java.awt.Rectangle;
import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.logging.Level;
import java.util.regex.Pattern;

import javax.swing.JList;
import javax.swing.JScrollPane;
import javax.swing.Popup;
import javax.swing.PopupFactory;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.text.BadLocationException;
import javax.swing.text.Caret;
import javax.swing.text.Document;
import javax.swing.text.Segment;

import quartz.lib.log.SimpleConsoleLogger;
import quartz.lib.scheme.SchemeEvaluator;
import quartz.lib.scheme.SchemeUtils;

public class KawapadContentAssist {
    static final SimpleConsoleLogger LOGGER = SimpleConsoleLogger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }
    private static final boolean DEBUG = false;
    private static final int HISTORY_SIZE = 512;
    Kawapad kawapad;
    Popup popup =null;
    LinkedList<String> history = new LinkedList<>();
    JList list=null;
    Pattern pat = Pattern.compile( "^[^a-zA-Z0-9].*$" );
    Comparator<String> comparator = new Comparator<String>() {
        @Override
        public int compare(String o1, String o2) {
            int i1= history.indexOf( o1 );
            int i2= history.indexOf( o2 );
                
            if ( i1 != i2 ) {
                return i2-i1;
            } else {
                boolean o1cat = pat.matcher( o1 ).matches();
                boolean o2cat = pat.matcher( o2 ).matches();
                if ( o1cat != o2cat ) {
                    if ( o2cat ) {
                        return -1;
                    } else {
                        return  1;
                    }
                }
                
                return o1.compareToIgnoreCase( o2 );
            }
        }
    };

    public KawapadContentAssist(Kawapad kawapad) {
        this.kawapad = kawapad;
    }
    public synchronized void hide() {
        if ( popup != null )
            popup.hide();
        popup = null;
        list = null;
    }
    public synchronized void pageTo( int direction ) {
        if ( list != null ) {
            moveTo( (list.getVisibleRowCount() -1) * direction );
        }
    }

    public synchronized void moveTo( int direction ) {
        if ( list != null ) {
            int index = list.getSelectedIndex() + direction;
            if ( index < 0 ) 
                index =0;
            
            int size = list.getModel().getSize();
            if ( size <= index )
                index = size-1;
                
            list.setSelectedIndex( index );
        }
    }
    public synchronized String get() {
        if ( list != null ) {
            String selectedValue = (String) list.getSelectedValue();
            if ( selectedValue == null ) {
                if ( list.getModel().getSize() != 0 )
                    selectedValue = (String) list.getModel().getElementAt( 0 );
            }
            return selectedValue;
        } else {
            return null;
        }
    }
    public synchronized void complete( Caret caret ) {
        Document document = kawapad.getDocument();
        int length = document.getLength();
        Segment text = KawapadSelection.getText( document );
        int dot = caret.getDot();
        if ( length < dot )
            dot = length;

        int rightEdgePos = dot == length ? length : 
                KawapadSelection.rightWordEdgePos( text, dot );
        int leftEdgePos = KawapadSelection.leftWordEdgePos( text, dot -1 );
        if ( rightEdgePos < 0 || leftEdgePos < 0 || rightEdgePos < leftEdgePos ) {
        } else {
            caret.setDot( leftEdgePos );
            caret.moveDot( rightEdgePos );
            String s = get();
            kawapad.replaceSelection( s );
            addHistory( s );
            hide();
        }
    }
    private void addHistory(String s) {
        history.remove( s );
        history.add( s );
        while ( HISTORY_SIZE  < history.size() ){
            history.pop();
        }
    }
    public synchronized void updatePopup(Caret caret) {
        SchemeEvaluator evaluator = kawapad.getSchemeEngine().getSchemeEvaluator();
        
        try {
            Document document = kawapad.getDocument();
            int length = document.getLength();
            Segment text = KawapadSelection.getText( document );
            int dot = caret.getDot();
            if ( length < dot )
                dot = length;

            int rightEdgePos = dot;
            int leftEdgePos = KawapadSelection.leftWordEdgePos( text, dot -1 );
            String currentWord;
            if ( rightEdgePos < 0 || leftEdgePos < 0 || rightEdgePos <= leftEdgePos ) {
                currentWord = "";
            } else {
                currentWord = String.valueOf( text.subSequence( leftEdgePos, rightEdgePos ) );
            }
            
            if ( DEBUG )
                System.err.println( "currentWord:" + currentWord );
            
            ArrayList<String> allKeys = 
                    new ArrayList<>( SchemeUtils.getAllKey( evaluator ) );

            ArrayList<String> keys = new ArrayList<>();
            for ( String key : allKeys ) {
                if ( key.startsWith( currentWord ) ) {
                    keys.add(key);
                }
            }
            
            keys.sort( comparator );
            
            if ( popup != null)
                popup.hide();
            
            Rectangle rectangle = kawapad.getUI().modelToView( kawapad, rightEdgePos );
            list = new JList( keys.toArray() );
            JScrollPane sp = new JScrollPane(list);
            sp.setSize( new Dimension( 300,300 ) );
            list.addListSelectionListener( new ListSelectionListener() {
                @Override
                public void valueChanged(ListSelectionEvent e) {
                    list.ensureIndexIsVisible( list.getSelectedIndex() );
                }
            } );

            PopupFactory popupFactory = PopupFactory.getSharedInstance();
            Point s = kawapad.getLocationOnScreen();
            popup = popupFactory.getPopup( kawapad, sp, s.x + rectangle.x, s.y+rectangle.y+rectangle.height + 1 );
            popup.show();
        } catch (BadLocationException e1) {
            e1.printStackTrace();
        }
    }
    
    
}
