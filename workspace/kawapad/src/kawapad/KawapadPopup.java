package kawapad;

import java.awt.Dimension;
import java.awt.Point;
import java.awt.Rectangle;
import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JList;
import javax.swing.JScrollPane;
import javax.swing.Popup;
import javax.swing.PopupFactory;
import javax.swing.text.BadLocationException;
import javax.swing.text.Caret;
import javax.swing.text.Document;
import javax.swing.text.Segment;

import gnu.mapping.LocationEnumeration;
import gnu.mapping.NamedLocation;
import kawa.standard.Scheme;
import pulsar.lib.scheme.SchemeUtils;
import pulsar.lib.secretary.SecretaryMessage;

public class KawapadPopup {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }
    private static final boolean DEBUG = false;
    Kawapad kawapad;
    Popup popup =null;
    JList list=null;
    public KawapadPopup(Kawapad kawapad) {
        this.kawapad = kawapad;
    }
    public synchronized void hide() {
        if ( popup != null )
            popup.hide();
        popup = null;
        list = null;
    }
    public synchronized void moveTo( int direction ) {
        if ( list != null ) {
            list.setSelectedIndex( list.getSelectedIndex() + direction );
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
        Segment text = SchemeParentheses.getText( document );
        int dot = caret.getDot();
        if ( length < dot )
            dot = length;

        int rightEdgePos = dot == length ? length : 
                SchemeParentheses.rightWordEdgePos( text, dot );
        int leftEdgePos = SchemeParentheses.leftWordEdgePos( text, dot -1 );
        if ( rightEdgePos < 0 || leftEdgePos < 0 || rightEdgePos <= leftEdgePos ) {
        } else {
            caret.setDot( leftEdgePos );
            caret.moveDot( rightEdgePos );
            kawapad.replaceSelection( get() );
            hide();
        }
    }
    public synchronized void updatePopup(Caret caret) {
        kawapad.getSchemeSecretary().executeWithoutSecretarially( new SecretaryMessage.NoReturnNoThrow<Scheme>() {
            @Override
            public void execute0(Scheme scheme, Object[] args) {
                try {
                    Document document = kawapad.getDocument();
                    int length = document.getLength();
                    Segment text = SchemeParentheses.getText( document );
                    int dot = caret.getDot();
                    if ( length < dot )
                        dot = length;

                    int rightEdgePos = dot;
                    int leftEdgePos = SchemeParentheses.leftWordEdgePos( text, dot -1 );
                    String currentWord;
                    if ( rightEdgePos < 0 || leftEdgePos < 0 || rightEdgePos <= leftEdgePos ) {
                        currentWord = "";
                    } else {
                        currentWord = String.valueOf( text.subSequence( leftEdgePos, rightEdgePos ) );
                    }
                    
                    if ( DEBUG )
                        System.err.println( "currentWord:" + currentWord );
                    
                    ArrayList<String> allKeys = new ArrayList<>();
                    LocationEnumeration en = scheme.getEnvironment().enumerateAllLocations();
                    for ( ; en.hasNext(); ) {
                        NamedLocation l= en.next();
                        String key = SchemeUtils.symbolToString( l.getKeySymbol() );
                        if ( key.startsWith( currentWord ) ) {
                            allKeys.add(key);
                        }
                    }
                    allKeys.sort( new Comparator<String>() {
                        @Override
                        public int compare(String o1, String o2) {
                            return o1.compareTo( o2 );
                        }
                    });
                    
                    if ( popup != null)
                        popup.hide();
                    
                    Rectangle rectangle = kawapad.getUI().modelToView( kawapad, rightEdgePos );
                    list = new JList( allKeys.toArray() );
                    JScrollPane sp = new JScrollPane(list);
                    sp.setSize( new Dimension( 300,300 ) );
                    PopupFactory popupFactory = PopupFactory.getSharedInstance();
                    Point s = kawapad.getLocationOnScreen();
                    popup = popupFactory.getPopup( kawapad, sp, s.x + rectangle.x, s.y+rectangle.y+rectangle.height + 1 );
                    popup.show();
                } catch (BadLocationException e1) {
                    e1.printStackTrace();
                }
            }
        });
    }
    
    
}
