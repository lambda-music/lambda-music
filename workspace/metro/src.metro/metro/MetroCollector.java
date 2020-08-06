package metro;


public interface MetroCollector<T> {
    void collect( T value );
    public static final MetroCollector NULL = new NullCollector<>();
}

class NullCollector<T> implements MetroCollector<T> {
    @Override
    public void collect(T value) {
    }
}
