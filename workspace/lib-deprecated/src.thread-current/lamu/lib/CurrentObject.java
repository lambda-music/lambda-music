package lamu.lib;

public final class CurrentObject<T> {
    private final String name;
    public CurrentObject(Class<T> clazz) {
        super();
        this.name = clazz.getSimpleName();
    }
    public CurrentObject(String name) {
        super();
        this.name = name;
    }
    private final ThreadLocal<T> threadLocal = new ThreadLocal<>();
    public void set( T current ) {
        threadLocal.set( current );
    }
    public T get() {
        T current = threadLocal.get();
        if ( current == null ) 
            throw new IllegalStateException( name + " is not present" );
        return current;
    }
    public boolean isPresent() {
        return threadLocal.get() != null;
    }
}
