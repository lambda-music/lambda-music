package lamu.lib.kawautils;

@FunctionalInterface
public interface SchemeNamedArgsConv<T> {
    T convert( Object object );
}
