package ats.pulsar.lib;

import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.JRadioButton;

public class JPulsarRadioButton extends JRadioButton implements JUserObjectContainer {
	public JPulsarRadioButton() {
		super();
	}
	public JPulsarRadioButton(Action a) {
		super(a);
	}
	public JPulsarRadioButton(Icon icon, boolean selected) {
		super(icon, selected);
	}
	public JPulsarRadioButton(Icon icon) {
		super(icon);
	}
	public JPulsarRadioButton(String text, boolean selected) {
		super(text, selected);
	}
	public JPulsarRadioButton(String text, Icon icon, boolean selected) {
		super(text, icon, selected);
	}
	public JPulsarRadioButton(String text, Icon icon) {
		super(text, icon);
	}
	public JPulsarRadioButton(String text) {
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
