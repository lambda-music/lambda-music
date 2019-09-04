package pulsar.lib.swing;

import java.awt.Component;

public class JPulsarUserObject extends Component {
    public JPulsarUserObject() {
    }
    public JPulsarUserObject(Object userObject) {
        super();
        this.userObject = userObject;
    }

    protected Object userObject;
    public Object getUserObject() {
        return userObject;
    }
    public void setUserObject( Object userObject ) {
        this.userObject = userObject;
    }
}
