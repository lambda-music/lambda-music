package ats.pulsar.lib;

public interface JUserObjectContainer {
	Object getUserObject();
	void setUserObject( Object userObject );
	public void setSelected( boolean selected );
	public boolean isSelected();
}
