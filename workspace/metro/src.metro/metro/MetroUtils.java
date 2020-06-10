package metro;

import java.util.Collection;
import java.util.Objects;

public class MetroUtils {
    public static String listToString( Collection<? extends Object> list ) {
        if ( list == null )
            return "#f";
        
        StringBuilder sb = new StringBuilder();
        int i =0;
        for ( Object o : list ) {
            if ( i++ != 0 )
                sb.append( " " );
            sb.append( Objects.toString(o));
        }
        return
            String.format( "'(%s)", sb.toString() );
    }
}
