package lamu.lib.args;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class ArgsNamedArgument {
    private static final Pattern parseArgPattern = Pattern.compile( "^--([a-zA-Z0-9\\_\\-]+)(\\=(.*))?$" );
    private String key;
    private String value;
    public ArgsNamedArgument( String key, String value ) {
        this.key = key;
        this.value = value;
    }

    public ArgsNamedArgument( String s ) {
        Matcher m = parseArgPattern.matcher( s );
        if ( m.matches() ) {
            this.key = m.group( 1 );
            this.value = m.group( 3 );
        } else {
            this.key = s;
            this.value = null;
        }
    }
    public String getKey() {
        return key;
    }
    public String getValue() {
        return value;
    }
    @Override
    public String toString() {
        return "{"+ key + "=>" + value + "}";
    }
}