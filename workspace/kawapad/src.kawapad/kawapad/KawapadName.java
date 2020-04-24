package kawapad;

public interface KawapadName {
    public String getName();
    public static String getCaption(Object o) {
        if ( o instanceof KawapadName ) { 
            return ((KawapadName)o).getName();
        } else {
            return o.toString();
        }
    }
}
