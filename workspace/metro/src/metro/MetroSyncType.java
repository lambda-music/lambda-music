package metro;

public enum MetroSyncType {
    IMMEDIATE, PARALLEL, SERIAL;
    public static MetroSyncType toSyncType( String value ) {
        if ( value == null ) throw new IllegalArgumentException();
        if ( "para".equals( value ) )
            return PARALLEL;
        else if ( "p".equals( value ) )
            return MetroSyncType.PARALLEL;
        else if ( "seri".equals( value ) )
            return MetroSyncType.SERIAL;
        else if ( "s".equals( value ) )
            return MetroSyncType.SERIAL;
        else if ( "imme".equals( value ) )
            return MetroSyncType.IMMEDIATE;
        else if ( "i".equals( value ) )
            return MetroSyncType.IMMEDIATE;
        else if ( "immediately".equals( value ) )
            return MetroSyncType.IMMEDIATE;
        else
            return MetroSyncType.valueOf( value.toUpperCase() );
    }
}