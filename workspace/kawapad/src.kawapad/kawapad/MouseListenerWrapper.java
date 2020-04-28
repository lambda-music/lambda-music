package kawapad;

import java.awt.Component;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.Arrays;
import java.util.List;

public class MouseListenerWrapper implements MouseListener {
    final MouseListener listener;
    public MouseListenerWrapper(MouseListener l) {
        super();
        this.listener = l;
    }
    public void mouseClicked(MouseEvent e) {
        if ( ! e.isConsumed() )
            listener.mouseClicked(e);
    }
    public void mousePressed(MouseEvent e) {
        if ( ! e.isConsumed() )
            listener.mousePressed(e);
    }
    public void mouseReleased(MouseEvent e) {
        if ( ! e.isConsumed() )
            listener.mouseReleased(e);
    }
    public void mouseEntered(MouseEvent e) {
        if ( ! e.isConsumed() )
            listener.mouseEntered(e);
    }
    public void mouseExited(MouseEvent e) {
        if ( ! e.isConsumed() )
            listener.mouseExited(e);
    }
    public static void wrap( Component c ) {
        for ( MouseListener l : c.getMouseListeners() ) {
            c.removeMouseListener(l);
            c.addMouseListener( new MouseListenerWrapper(l));
        }
    }
    public static void addMouseListenerFirst( Component c, MouseListener listener ) {
        List<MouseListener> list = Arrays.asList(c.getMouseListeners());
        for ( MouseListener l : list ) {
            c.removeMouseListener(l);
        }
        c.addMouseListener(listener);
        for ( MouseListener l : list ) {
            c.addMouseListener(l);
        }
    }
}