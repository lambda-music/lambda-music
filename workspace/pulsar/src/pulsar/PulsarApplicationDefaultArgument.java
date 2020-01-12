package pulsar;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class PulsarApplicationDefaultArgument {
    public static class Element {
        String key;
        String value;
        public String getKey() {
            return key;
        }
        public String getValue() {
            return value;
        }
        public Element(String key, String value) {
            super();
            this.key = key;
            this.value = value;
        }
        
        public List<String> getValueAsList() {
            ArrayList<String> list = new ArrayList<>( Arrays.asList( this.value.split( "[\\s]+" ) ) );
            return list;
        }
        public List<String> interpolate( String fileName ) {
            List<String> list = getValueAsList();
            interpolate( list, fileName );
            return list;
        }
        public static void interpolate( List<String> valueAsList, String fileName ) {
            valueAsList.replaceAll( (s)-> "$*".equals( s ) ? fileName : s );
            valueAsList.removeIf((s)-> s == null || "".equals( s.trim() )  );
        }
    }
    public static File getInitFile() {
        return new File( System.getProperty("user.home"), ".kawapad/kawapad-default-arguments.conf" );
    }

    public static List<Element> load() throws IOException {
        if ( getInitFile().exists() && getInitFile().isFile() ) {
        } else {
            return Collections.emptyList();
        }
        
        try ( BufferedReader r = new BufferedReader( new FileReader( getInitFile() ) ) ) {
            List<Element> result = new ArrayList<>();
            for (;;){
                String s = r.readLine();
                if ( s == null )
                    break;
                
                String[] ts = s.split( "\t" );
                
                String key =null;
                String value =null;
                if ( 0 < ts.length ) {
                    key = ts[0];
                }
                if ( 1 < ts.length ) {
                    value = ts[1];
                }
                if ( key != null && value != null ) {
                    result.add( new Element( key, value ) );
                }
            }
            return result;
        }
    }
    
}
