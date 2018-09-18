package ats.pulsar.lib;

import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.JCheckBox;

public class JPulsarCheckBox extends JCheckBox implements JUserObjectContainer {
	public JPulsarCheckBox() {
		super();
	}
	public JPulsarCheckBox(Action a) {
		super(a);
	}
	public JPulsarCheckBox(Icon icon, boolean selected) {
		super(icon, selected);
	}
	public JPulsarCheckBox(Icon icon) {
		super(icon);
	}
	public JPulsarCheckBox(String text, boolean selected) {
		super(text, selected);
	}
	public JPulsarCheckBox(String text, Icon icon, boolean selected) {
		super(text, icon, selected);
	}
	public JPulsarCheckBox(String text, Icon icon) {
		super(text, icon);
	}
	public JPulsarCheckBox(String text) {
		super(text);
	}
	transient Object userObject=null;
	@Override
	public Object getUserObject() {
		return userObject;
	}
	@Override
	public void setUserObject(Object userObject) {
		this.userObject = userObject;
	}
}
