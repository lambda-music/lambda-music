package lamu;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

class LamuNamedArgument {
    static final Pattern parseArgPattern = Pattern.compile( "^--([a-zA-Z0-9\\_\\-]+)\\=(.*)$" );
    private String key;
    private String value;
    LamuNamedArgument( String key, String value ) {
        this.key = key;
        this.value = value;
    }

    LamuNamedArgument( String s ) {
        Matcher m = parseArgPattern.matcher( s );
        if ( m.matches() ) {
            this.key = m.group( 1 );
            this.value = m.group( 2 );
        } else {
            this.key = s;
            this.value = null;
        }
    }
    String getKey() {
        return key;
    }
    String getValue() {
        return value;
    }
    @Override
    public String toString() {
        return "{"+ key + "=>" + value + "}";
    }
}