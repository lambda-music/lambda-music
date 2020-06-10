package metro;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public interface MetroTrackSelectorFactory {
    public abstract MetroTrackSelector create( Object[] args );

    /**
     * 
     */
    
    public static <T> List<T> processArgs( Object[] args ) {
        List<T> result = new ArrayList<T>();
        for (Object o : args ) {
            if ( o instanceof Collection ) {
                 result.addAll(((Collection<T>)o));
            } else {
                result.add((T) o );
            }
        }
        return result;
    }
        
    static void checkArgs( Object[] args, int numArgs ) {
        if ( args == null )
            throw new NullPointerException( "the argument array is null" );
        if ( args.length != numArgs )
            throw new IllegalArgumentException( "the argument number("+args.length+") != " + numArgs );
    
    }
    
    static void checkArgs( Object[] args ) {
        if ( args == null )
            throw new NullPointerException( "the argument array is null" );
    }
    
    static void checkArgsMin( Object[] args, int numArgs ) {
        if ( args == null )
            throw new NullPointerException( "the argument array is null" );
        if ( args.length < numArgs )
            throw new IllegalArgumentException( "the argument number("+args.length+") < " + numArgs );
    }
}
