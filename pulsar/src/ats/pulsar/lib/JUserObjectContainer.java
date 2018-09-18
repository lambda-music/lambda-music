package ats.pulsar.lib;

public interface JUserObjectContainer {
	Object getUserObject();
	void setUserObject( Object userObject );
	class Default implements JUserObjectContainer {
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
}
