package lamu.utils.lib;


import java.awt.BorderLayout;
import java.awt.Cursor;
import java.awt.Point;
import java.awt.event.InputEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.JComponent;
import javax.swing.JFormattedTextField;
import javax.swing.JFrame;
import javax.swing.JSpinner;
import javax.swing.JSpinner.DefaultEditor;
import javax.swing.SpinnerModel;

public class JSpinnerDragger {
    private float multiplier;
    public JSpinnerDragger(final JSpinner spinner, float multiplier) {
        this.multiplier = multiplier;
        makeDraggable(spinner);
        spinner.addPropertyChangeListener("editor", new PropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent evt) {
                makeDraggable(spinner);
            }
        });
    }

    private void makeDraggable(JSpinner spinner) {
        JComponent editor = spinner.getEditor();
        if (editor instanceof DefaultEditor) {
            DefaultEditor                  defaultEditor = (DefaultEditor) editor;
            JFormattedTextField            textField     = defaultEditor.getTextField();
            DraggableSpinnerUIMouseHandler handler       = new DraggableSpinnerUIMouseHandler(spinner, false);
            textField.setCursor(Cursor.getPredefinedCursor(Cursor.E_RESIZE_CURSOR));
            textField.addMouseListener(handler);
            textField.addMouseMotionListener(handler);
        }
    }

    public static JSpinner install(JSpinner spinner) {
        return install(spinner, 0);
    }

    public static JSpinner install(JSpinner spinner, float multiplier) {
        new JSpinnerDragger(spinner, multiplier);
        return spinner;
    }

    private class DraggableSpinnerUIMouseHandler implements MouseListener, MouseMotionListener {
        private Point    mousePoint = new Point();
        private boolean bothDirection = true;
        private boolean  vertical;
        private boolean  enabled;
        private JSpinner spinner;

        private DraggableSpinnerUIMouseHandler(JSpinner spinner, boolean vertical) {
            this.vertical = vertical;
            this.spinner  = spinner;
        }

        public void mousePressed(MouseEvent e) {
            enabled = false;
            mousePoint.setLocation(e.getPoint());
        }

        public void mouseDragged(MouseEvent e) {
            int delta = 0;
            if ( bothDirection ) {
                delta =
                    (mousePoint.y - e.getPoint().y) +
                    ( -(mousePoint.x - e.getPoint().x) );
            } else {
                if (vertical) {
                    delta = mousePoint.y - e.getPoint().y;
                } else {
                    delta = -(mousePoint.x - e.getPoint().x);
                }
            }

            int absDelta = Math.abs(delta);
            if (!enabled && absDelta < 5) {
                return;
            }
            if (!enabled) {
                enabled = true;
                mousePoint.setLocation(e.getPoint());
                return;
            }
            if ((e.getModifiers() & InputEvent.CTRL_MASK) != 0) {
                absDelta = absDelta * 10;
            }
            if (multiplier > 0) {
                absDelta = (int) (absDelta * multiplier);
            }
            if (absDelta == 0) {
                return;
            }
            SpinnerModel model = this.spinner.getModel();
            Object       value = model.getValue();
            for (int i = 0; i < absDelta; i++) {
                if (delta < 0) {
                    value = model.getPreviousValue();
                } else {
                    value = model.getNextValue();
                }
                if (value != null) {
                    model.setValue(value);
                } else {
                    break;
                }
            }
            mousePoint.setLocation(e.getPoint());
        }

        public void mouseClicked(MouseEvent e) {}
        public void mouseReleased(MouseEvent e) {}
        public void mouseMoved(MouseEvent e) {}
        public void mouseEntered(MouseEvent e) {}
        public void mouseExited(MouseEvent e) {}
    }

    public static void main(String[] args) {
        JFrame   frame   = new JFrame();
        JSpinner spinner = new JSpinner();

        // Install the draggable JSpinner
        JSpinnerDragger.install(spinner);

        frame.add(spinner, BorderLayout.NORTH);
        frame.setLocationRelativeTo(null);
        frame.pack();
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setVisible(true);
    }
};
