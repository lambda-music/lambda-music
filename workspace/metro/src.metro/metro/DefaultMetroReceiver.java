package metro;

import java.util.LinkedList;
import java.util.List;

public class DefaultMetroReceiver<T> implements MetroCollector<T> {
    private final List<T> list;
    public DefaultMetroReceiver() {
        this.list = new LinkedList<T>();
    }
    public DefaultMetroReceiver(List<T> list) {
        super();
        this.list = list;
    }
    public List<T> getResult() {
        return list;
    }
    @Override
    public void collect(T value) {
        if ( value != null )
            this.list.add( value );
    }
}