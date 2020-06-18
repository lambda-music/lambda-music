package lamu.utils.lib;

import java.awt.Container;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JSpinner;
import javax.swing.SpinnerModel;
import javax.swing.WindowConstants;

/**
 * This code is from:<br/>
 * <a href="https://searchcode.com/codesearch/view/12670719/">https://searchcode.com/codesearch/view/12670719/</a><br/>
 * You can watch its demonstration at <br/>
 * <a href="https://www.youtube.com/watch?v=iNlmKQK3ZvA">https://www.youtube.com/watch?v=iNlmKQK3ZvA</a>.<br/>
 *
 */
public class JDraggableSpinner extends JSpinner {
    public JDraggableSpinner() {
        super();
    }
    public JDraggableSpinner(SpinnerModel model) {
        super(model);
    }
    public static void main(String[] args) {
        JFrame frame = new JFrame();
        Container pane = frame.getContentPane();
        pane.setLayout( new BoxLayout( pane, BoxLayout.LINE_AXIS ));
        pane.add( new JDraggableSpinner() );
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
