/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package pulsar.lib.swing;

import java.awt.Color;
import java.awt.Event;
import java.awt.event.KeyEvent;

import javax.swing.InputMap;
import javax.swing.JComponent;
import javax.swing.JTextPane;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.DefaultEditorKit;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyledDocument;

/**
 * @author haya
 */
public class LispTextPaneWrapper {
    public static void setup( JTextPane textPane ) {
        setupEmacsBind( textPane );
        setupParenthesisMarker( textPane );
    }
    public static void setupEmacsBind( JTextPane textPane ) {
        textPane.setInputMap(JComponent.WHEN_FOCUSED, prepareKeyBindings() );
    }

    public static void setupParenthesisMarker(JTextPane textPane) {
        textPane.getDocument().addDocumentListener( new LispDocumentListener( textPane ) );
        textPane.addCaretListener( new LispCaretListener( textPane ));
    }
    static class LispCaretListener implements CaretListener {
        JTextPane textPane;
        public LispCaretListener(JTextPane textPane) {
            super();
            this.textPane = textPane;
        }
        public void caretUpdate(CaretEvent e) {
            invokeUpdateMarker(textPane);
        }
    }
    static class LispDocumentListener implements DocumentListener {
        JTextPane textPane;
        public LispDocumentListener(JTextPane textPane) {
            this.textPane = textPane;
        }
        public void insertUpdate(DocumentEvent e) {
            invokeUpdateMarker(textPane);
        }
        public void removeUpdate(DocumentEvent e) {
            invokeUpdateMarker(textPane);
        }
        public void changedUpdate(DocumentEvent e) {
            return;
        }
    }
    static void invokeUpdateMarker(final JTextPane textPane) {
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                updateMarker(textPane);
            }
        });
    }

    static void updateMarker(JTextPane textPane) {
        String text = textPane.getText();
        StyledDocument sdoc = textPane.getStyledDocument();
        SimpleAttributeSet plane = new SimpleAttributeSet();
        sdoc.setCharacterAttributes(0, text.length(), plane, true); // clear

        int caret_pos = textPane.getCaretPosition();
        if (caret_pos <= text.length() - 1
                && text.charAt(caret_pos) == '(') {
            int paren_depth = 0;
            int open_pos = caret_pos;
            int close_pos = -1;
            for (int i = caret_pos + 1; i < text.length(); i++) {
                switch (text.charAt(i)) {
                    case '(':
                        paren_depth++;
                        break;
                    case ')':
                        if (paren_depth == 0) {
                            close_pos = i;
                            markParen(textPane, open_pos, close_pos);
                            break;
                        }
                        paren_depth--;
                        break;
                }
            }
        }
        
        if (caret_pos > 0 && text.charAt(caret_pos - 1) == ')') {
            int paren_depth = 0;
            int open_pos = -1;
            int close_pos = caret_pos - 1;
            for (int i = caret_pos - 2; i >= 0; i--) {
                char c = text.charAt(i);
                switch (c) {
                    case ')':
                        paren_depth++;
                        break;
                    case '(':
                        if (paren_depth == 0) {
                            open_pos = i;
                            markParen(textPane, open_pos, close_pos);
                            break;
                        }
                        paren_depth--;
                        break;
                }
            }
        }
    }

    static void markParen(JTextPane textPane, int open_pos, int close_pos) {
        SimpleAttributeSet attr = new SimpleAttributeSet();
        StyleConstants.setBackground( attr, Color.LIGHT_GRAY );
        StyledDocument doc = (StyledDocument) textPane.getDocument();
        doc.setCharacterAttributes(open_pos, 1, attr, true);
        doc.setCharacterAttributes(close_pos, 1, attr, false);
        attr = new SimpleAttributeSet();
        //            StyleConstants.setBold(attr, true);
        doc.setCharacterAttributes(open_pos, close_pos - open_pos + 1, attr, false);
    }

    static InputMap prepareKeyBindings() {
        InputMap normal_map = null;
        InputMap ctrl_x_map = null;

        normal_map = (new JTextPane()).getInputMap();
        // Ctrl-b
        normal_map.put(KeyStroke.getKeyStroke(KeyEvent.VK_B, Event.CTRL_MASK),
                DefaultEditorKit.backwardAction);
        // Ctrl-f
        normal_map.put(KeyStroke.getKeyStroke(KeyEvent.VK_F, Event.CTRL_MASK),
                DefaultEditorKit.forwardAction);
        // Ctrl-p
        normal_map.put(KeyStroke.getKeyStroke(KeyEvent.VK_P, Event.CTRL_MASK),
                DefaultEditorKit.upAction);
        // Ctrl-n
        normal_map.put(KeyStroke.getKeyStroke(KeyEvent.VK_N, Event.CTRL_MASK),
                DefaultEditorKit.downAction);
        // Ctrl-a
        normal_map.put(KeyStroke.getKeyStroke(KeyEvent.VK_A, Event.CTRL_MASK),
                DefaultEditorKit.beginLineAction);
        // Ctrl-e
        normal_map.put(KeyStroke.getKeyStroke(KeyEvent.VK_E, Event.CTRL_MASK),
                DefaultEditorKit.endLineAction);
        // Ctrl-h
        normal_map.put(KeyStroke.getKeyStroke(KeyEvent.VK_H, Event.CTRL_MASK),
                DefaultEditorKit.deletePrevCharAction);
        // Ctrl-d
        normal_map.put(KeyStroke.getKeyStroke(KeyEvent.VK_D, Event.CTRL_MASK),
                DefaultEditorKit.deleteNextCharAction);
        // Ctrl-w
        normal_map.put(KeyStroke.getKeyStroke(KeyEvent.VK_W, Event.CTRL_MASK),
                DefaultEditorKit.cutAction);
        // Alt-w
        normal_map.put(KeyStroke.getKeyStroke(KeyEvent.VK_W, Event.ALT_MASK),
                DefaultEditorKit.copyAction);
        // Ctrl-y
        normal_map.put(KeyStroke.getKeyStroke(KeyEvent.VK_Y, Event.CTRL_MASK),
                DefaultEditorKit.pasteAction);
        // Ctrl-f
        normal_map.put(KeyStroke.getKeyStroke(KeyEvent.VK_F, Event.CTRL_MASK),
                DefaultEditorKit.forwardAction);
        {
            ctrl_x_map = (new JTextPane()).getInputMap();
            // C-x C-h
            ctrl_x_map.put(KeyStroke.getKeyStroke(KeyEvent.VK_H, Event.CTRL_MASK),
                    DefaultEditorKit.selectAllAction);
        }
        return normal_map;
    }
}

