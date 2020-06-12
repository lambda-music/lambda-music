package lamu.lib.threads;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

public class LamuThreadLocal<T> extends ThreadLocal<T> {
    private static List<LamuThreadLocal<? extends Object>> allInstances = new ArrayList<>();
    synchronized static void created( LamuThreadLocal<? extends Object> object ) {
        allInstances.add(object);
    }
    synchronized static void destroyed( LamuThreadLocal<? extends Object> object ) {
        allInstances.remove(object);
    }
    synchronized static List<LamuThreadLocal<? extends Object>> getAll() {
        return Collections.synchronizedList(allInstances); 
    }
    public static void preserve( Map<LamuThreadLocal<? extends Object>,Object> threadLocalMap ) {
        List<LamuThreadLocal<? extends Object>> all = getAll();
        for ( LamuThreadLocal<? extends Object> o :  all ) {
            threadLocalMap.put( o, o.get() ); 
        }
    }
    public static void restore( Map<LamuThreadLocal<? extends Object>,Object> threadlocalMap ) {
        for ( Map.Entry<LamuThreadLocal<? extends Object>, ? extends Object> e : threadlocalMap.entrySet() ) {
            LamuThreadLocal key = e.getKey();
            key.set( e.getValue() );
        }
    }
    {
        created( this );
    }
    public LamuThreadLocal() {
        super();
    }
}
