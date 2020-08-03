package lamu.lib.kawautils;

import java.util.List;
import java.util.Map;

@FunctionalInterface
public interface SchemeNamedArgsRef<T> {
    public abstract T getValue( Map<String,Object> namedArgs, List<Object> numberedArgs );
}