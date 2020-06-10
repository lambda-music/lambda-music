package metro;

import java.util.HashMap;

public class MetroFactoryMap<T> {
    private HashMap<String,T> factoryMap = new HashMap<>();
    public T getFactory( String name ) {
        if ( ! factoryMap.containsKey(name))
            throw new IllegalArgumentException("unknown factory name (" + name + ")");
        return factoryMap.get(name);
    }
    public void addFactory( String name, T factory ) {
        if ( factoryMap.containsKey(name))
            throw new IllegalArgumentException("duplicate factory name (" + name + ")");
        factoryMap.put( name, factory );
    }
}
