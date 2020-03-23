package lamu.lib.scheme;

public interface NameCaptionHolder {
    abstract String getNameCaption();
    public static String getCaption(Object o) {
        if ( o == null ) {
            return "null";
        } else if ( o instanceof NameCaptionHolder ) { 
            return ((NameCaptionHolder)o).getNameCaption();
        } else {
            return o.toString();
        }
    }

}
