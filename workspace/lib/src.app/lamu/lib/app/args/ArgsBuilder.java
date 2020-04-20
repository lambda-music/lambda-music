package lamu.lib.app.args;

import java.util.Deque;
import java.util.List;

public interface ArgsBuilder {
    <T> Deque<T> getValueStack( ArgsBuilderStackKey<T> key );
    void registerFactory(String key, ArgsBuilderElementFactory value);
    ArgsBuilderElementFactory getFactory(String key);
    void parse( List<String> arguments );
}