package kawapad;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;

public class SyntaxElementList implements Iterable<SyntaxElement> {
    private final ArrayList<SyntaxElement> list;
    private final HashMap<Object,SyntaxElement> map;
    public SyntaxElementList( Collection<SyntaxElement> c ) {
        this.list = new ArrayList<>( c );
        this.map = new HashMap<>();
        for ( SyntaxElement e : this.list ) {
            this.map.put( e.getName(), e );
        }
    }
    public SyntaxElement get( Object name ) {
        SyntaxElement e =  this.map.get( name );
        if ( e == null )
            throw new IllegalArgumentException( "could not find " + name + "." );
        else
            return e;
    }
    @Override
    public Iterator<SyntaxElement> iterator() {
        return Collections.unmodifiableList( this.list ).iterator();
    }
}