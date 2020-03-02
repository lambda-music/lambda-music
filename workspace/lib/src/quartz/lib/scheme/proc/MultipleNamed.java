package quartz.lib.scheme.proc;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import gnu.mapping.PropertySet;
import gnu.mapping.Symbol;

public interface MultipleNamed {
    static final Symbol MULTIPLE_NAME_KEY = Symbol.valueOf( "multiple-names" );
    public static void setNames( PropertySet self, Collection<String> names ) {
        List<String> l;
        try {
            l = (List<String>) self.getProperty( MULTIPLE_NAME_KEY , null );
        } catch ( ClassCastException e ) {
            e.printStackTrace();
            l = null;
        }
        if ( l == null ) {
            l = new ArrayList( names );
            self.setProperty( MULTIPLE_NAME_KEY, l );
        }
        
        if ( 0 < l.size() ) {
            self.setName( l.get( 0 ) );
        }
    }
    public static List<String> getNames( PropertySet self ) {
        List<String> l = (List<String>) self.getProperty( MULTIPLE_NAME_KEY , null );
        return Collections.unmodifiableList( l );
    }

    default PropertySet getPropertySet() {
        return ((PropertySet)this);
    };

    default void setNames( String ... names ) {
        setNames( getPropertySet(), Arrays.asList( names ) );
    }
    default void setNames( List<String> names ) {
        setNames( getPropertySet(), names );
    }

    default List<String> getNames() {
        return getNames( getPropertySet() );
    }
}
