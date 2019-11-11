package pulsar.lib;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

public class Version {
    private static String version;
    public static String get( Class<?> c ) {
        if ( version != null ) {
            return version;
        }
        InputStream in = c.getResourceAsStream( "version.txt" );
        try {
            BufferedReader r = new BufferedReader( new InputStreamReader( in ) );
            version = r.readLine();
            r.close();
            
            return version;
        } catch ( IOException e ) {
            e.printStackTrace();
        }
        return "unknown";
    }
}
