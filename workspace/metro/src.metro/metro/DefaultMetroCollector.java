package metro;

import java.util.ArrayList;
import java.util.List;

public class DefaultMetroCollector<T> implements MetroCollector<T> {
    private final List<T> list;
    public DefaultMetroCollector() {
        this.list = new ArrayList<T>();
    }
    public DefaultMetroCollector(List<T> list) {
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