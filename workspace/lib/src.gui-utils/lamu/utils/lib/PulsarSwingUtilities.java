package lamu.utils.lib;

import java.awt.Container;
import java.awt.Dimension;
import java.awt.Insets;

import javax.swing.JComponent;

public class PulsarSwingUtilities {
    public static Dimension getInnerSize( Container c ) {
        Dimension size = c.getSize();
        if ( c instanceof JComponent ) {
            Insets insets = ((JComponent)c).getBorder().getBorderInsets(c);
            size.width  -= insets.left + insets.right;
            size.height -= insets.top+ + insets.bottom;
        }
        return size;
    }
}
