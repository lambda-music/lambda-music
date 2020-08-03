package metro;

public enum MetroTrackMode {
	MONO, POLY;
	public static MetroTrackMode fromString( String s ) {
	    return valueOf( s.trim().toUpperCase() );
	}
}
