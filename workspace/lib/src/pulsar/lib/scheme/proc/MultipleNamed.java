package pulsar.lib.scheme.proc;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import gnu.mapping.PropertySet;
import gnu.mapping.Symbol;

public interface MultipleNamed {
    static final Symbol MULTIPLE_NAME_KEY = Symbol.valueOf( "multiple-names" );
    public static void setNames( PropertySet self, String... names ) {
        List<String> o;
        try {
            o = (List<String>) self.getProperty( MULTIPLE_NAME_KEY , null );
        } catch ( ClassCastException e ) {
            e.printStackTrace();
            o = null;
        }
        if ( o == null ) {
            o = new ArrayList( Arrays.asList( names ));
            self.setProperty( MULTIPLE_NAME_KEY, o );
        }
    }
    public static List<String> getNames( PropertySet self ) {
        List<String> o = (List<String>) self.getProperty( MULTIPLE_NAME_KEY , null );
        return Collections.unmodifiableList( o );
    }

    default PropertySet getPropertySet() {
        return ((PropertySet)this);
    };

    default void setNames( String ... names ) {
        setNames( getPropertySet(), names );
    }

    default List<String> getNames() {
        return getNames( getPropertySet() );
    }

}
