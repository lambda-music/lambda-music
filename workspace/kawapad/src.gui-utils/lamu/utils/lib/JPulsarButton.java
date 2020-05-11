package lamu.utils.lib;

import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.JButton;

public class JPulsarButton extends JButton implements JUserObjectContainer {
    public JPulsarButton() {
        super();
    }
    public JPulsarButton(Action a) {
        super(a);
    }
    public JPulsarButton(Icon icon) {
        super(icon);
    }
    public JPulsarButton(String text, Icon icon) {
        super(text, icon);
    }
    public JPulsarButton(String text) {
        super(text);
    }
    volatile Object userObject=null;
    @Override
    public Object getUserObject() {
        return userObject;
    }
    @Override
    public void setUserObject(Object userObject) {
        this.userObject = userObject;
    }
}
