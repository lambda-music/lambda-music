package quartz.lib.scheme;

public interface HasName {
    abstract String getName();
    public static String getCaption(Object o) {
        if ( o == null ) {
            return "null";
        } else if ( o instanceof HasName ) { 
            return ((HasName)o).getName();
        } else {
            return o.toString();
        }
    }

}
