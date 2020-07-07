package kawapad;

import java.awt.BasicStroke;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.HierarchyEvent;
import java.awt.event.HierarchyListener;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.lang.invoke.MethodHandles;
import java.util.logging.Level;

import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.JTextField;
import javax.swing.Popup;
import javax.swing.PopupFactory;
import javax.swing.SwingUtilities;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.Caret;
import javax.swing.text.Document;
import javax.swing.text.Segment;

import lamu.lib.logging.Logger;

public class KawapadSearchBox implements ComponentListener, HierarchyListener {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }

    private Kawapad kawapad;
    public KawapadSearchBox( Kawapad kawapad ) {
        this.kawapad = kawapad;
        this.kawapad.addHierarchyListener( this );
    }
    
    private final class SearchTextFieldListener implements DocumentListener, KeyListener {
        @Override
        public void removeUpdate(DocumentEvent e) {
//            logInfo("removeUpdate:" + e);
            KawapadTemporaryParenthesisHighlighter.forceClearHighlightedParenthesis();
            Kawapad.highlightSpecificWord(kawapad, getText() );
        }

        @Override
        public void insertUpdate(DocumentEvent e) {
//            logInfo("insertUpdate:" + e);
            KawapadTemporaryParenthesisHighlighter.forceClearHighlightedParenthesis();
            Kawapad.highlightSpecificWord(kawapad, getText() );
        }

        @Override
        public void changedUpdate(DocumentEvent e) {
//            logInfo("changedUpdate:" + e);
            KawapadTemporaryParenthesisHighlighter.forceClearHighlightedParenthesis();
            Kawapad.highlightSpecificWord(kawapad, getText() );
        }

        @Override
        public void keyPressed(KeyEvent e) {
            logInfo( "keyPressed" + e  );
            if ( (e.getModifiers() & InputEvent.CTRL_MASK) != 0 ) {
            	switch ( e.getKeyCode() ) {
            	case KeyEvent.VK_COMMA :
            		e.consume();
            		callAction(kawapad.SEARCH_PREV_ACTION);
            		break;
            	case KeyEvent.VK_PERIOD :
            		e.consume();
            		callAction(kawapad.SEARCH_NEXT_ACTION);
            		break;
            	case KeyEvent.VK_ESCAPE :
            		KawapadSearchBox.this.hideSearchBox();
            		break;
            	}
            } else {
            	switch ( e.getKeyCode() ) {
            	case KeyEvent.VK_ENTER :
            		kawapad.requestFocus();
            		break;
            	case KeyEvent.VK_UP :
            		e.consume();
            		callAction(kawapad.SEARCH_PREV_ACTION);
            		break;
            	case KeyEvent.VK_DOWN :
            		e.consume();
            		callAction(kawapad.SEARCH_NEXT_ACTION);
            		break;
            	case KeyEvent.VK_ESCAPE :
            		KawapadSearchBox.this.hideSearchBox();
            		break;
            	}
            }
        }

		private void callAction(Action action) {
			action.actionPerformed( new ActionEvent(kawapad, (int) System.currentTimeMillis(), "" ) );
    		Kawapad.highlightSpecificWord( kawapad, getText() );
		}
        @Override
        public void keyTyped(KeyEvent e) {
        }
        @Override
        public void keyReleased(KeyEvent e) {
        }
    }
    SearchTextFieldListener searchTextFieldListener = new SearchTextFieldListener();
    
    final JTextField searchTextField = createSearchTextField();
    public JTextField createSearchTextField() {
        JTextField field = new JTextField( "" );
        field.setBorder( BorderFactory.createCompoundBorder(
            BorderFactory.createStrokeBorder( new BasicStroke(1) ), 
            BorderFactory.createEmptyBorder(0,10,0,10) ) );
        field.getDocument().addDocumentListener( searchTextFieldListener );
        field.addKeyListener(searchTextFieldListener);
        field.setEditable(true);
        field.setFocusable(true);
        return field;
    }

    boolean visible=false;
    public void setVisible(boolean visible) {
        this.visible = visible;
        this.updateStatusField();
    }
    public boolean isVisible() {
        return visible;
    }
    public String getText() {
        return this.searchTextField.getText();
    }
    public void setText(String text) {
        this.searchTextField.setText( text );
    }

    private final PopupFactory popupFactory = PopupFactory.getSharedInstance();
    
    public static final int SEARCH_BOX_WIDTH_MARGIN = 100;
    public static final int SEARCH_BOX_HEIGHT = 30;
    
    Popup searchTextFieldPopup; 
    public synchronized void updateStatusField() {
        if ( searchTextFieldPopup != null ) {
            searchTextFieldPopup.hide();
            searchTextFieldPopup = null;
        }
        
        if ( ! this.visible ) {
            return;
        }

        Container parent = kawapad.getParent();
        if ( parent == null ) {
            logWarn("kawapad has no parent");
        }
        
        this.searchTextField.setPreferredSize( new Dimension( parent.getWidth() / 2,  SEARCH_BOX_HEIGHT ));
        Point loc = new Point(parent.getWidth() /2, SEARCH_BOX_HEIGHT * -1 );
        SwingUtilities.convertPointToScreen( loc, parent );
        Rectangle size = kawapad.getVisibleRect();
        logInfo ( "" + size.height );
        this.searchTextFieldPopup = popupFactory.getPopup( kawapad, searchTextField, loc.x, loc.y + size.height + 0  );
        this.searchTextFieldPopup.show();
    }

    Container lastParent = null;
    @Override
    public void hierarchyChanged(HierarchyEvent e) {
        if ( this.lastParent != null ) {
            this.lastParent.removeComponentListener( this );
        }
        Container parent = e.getChangedParent();
        if ( parent != null ) {
            parent.addComponentListener( this );
            this.lastParent = parent; 
        }
    }
    
    @Override
    public void componentResized(ComponentEvent e) {
        updateStatusField();
    }
    @Override
    public void componentMoved(ComponentEvent e) {
    }
    @Override
    public void componentShown(ComponentEvent e) {
    }
    @Override
    public void componentHidden(ComponentEvent e) {
    }
    
    
    public void showSearchBox() {
        Document document = kawapad.getDocument();
        Segment text = KawapadSelection.getText( document );
        Caret caret = kawapad.getCaret();
        String word = KawapadTemporarySearchHighlighter.getCurrentWord( text, caret );
        if ( word == null ) {
        } else {
            this.setText( word );
        }
        this.setVisible( true  );
        this.searchTextField.requestFocus(true);
        logInfo("requestFocus");
    }
    public void hideSearchBox() {
        this.setVisible( false );
        this.kawapad.requestFocus();
        logInfo("kawapad.requestFocus");
    }
}
