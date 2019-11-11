package metro;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

public interface MetroCollector<T> {
    void add( T value );
    List<T> get();
    public class Default<T> implements MetroCollector<T> {
        private final List<T> list;
        public Default() {
            this.list = new LinkedList<T>();
        }
        public Default(List<T> list) {
            super();
            this.list = list;
        }
        @Override
        public List<T> get() {
            return list;
        }
        @Override
        public void add(T value) {
            if ( value != null )
                this.list.add( value );
        }
    }
    public class NullCollector<T> implements MetroCollector<T> {
        public NullCollector() {
        }
        @Override
        public List<T> get() {
            return Collections.EMPTY_LIST;
        }
        @Override
        public void add(T value) {
        }
    }
    public static final MetroCollector NULL = new NullCollector<>();
}
