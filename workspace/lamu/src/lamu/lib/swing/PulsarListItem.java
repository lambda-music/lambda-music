package lamu.lib.swing;

public class PulsarListItem {
    private String caption;
    private Object userObject;
    public PulsarListItem(String caption, Object user) {
        super();
        this.caption = caption;
        this.userObject = user;
    }
    public String getCaption() {
        return caption;
    }
    public void setCaption(String caption) {
        this.caption = caption;
    }
    public Object getUserObject() {
        return userObject;
    }
    public void setUserObject(Object user) {
        this.userObject = user;
    }
    @Override
    public String toString() {
        return caption;
    }
//  @Override
//  public boolean equals(Object obj) {
//      if (obj instanceof PulsarListItem) {
//          Object user = ((PulsarListItem)obj).userObject;
//          return ( this.userObject == user ) || ( this.userObject.equals( user ) );
//      } else {
//          return false;
//      }
//  }
}
