package lamu.utils.lib;

import java.awt.Container;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JSpinner;
import javax.swing.WindowConstants;

public class JDraggableSpiner extends JSpinner {
    public static void main(String[] args) {
        JFrame frame = new JFrame();
        Container pane = frame.getContentPane();
        pane.setLayout( new BoxLayout( pane, BoxLayout.LINE_AXIS ));
        pane.add( new JDraggableSpiner() );
        pane.add( new JButton("aa"));
        pane.doLayout();
        frame.setDefaultCloseOperation( WindowConstants.EXIT_ON_CLOSE );
        frame.setSize(400, 400);
        frame.setVisible(true);
    }
    {
        JSpinnerDragger.install(this);
    }
}
