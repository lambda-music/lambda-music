package lamu.lib.app.args;

import java.util.Deque;
import java.util.List;

public interface ArgumentParser {
    <T> Deque<T> getValueStack( ArgumentParserStackKey<T> key );
    void registerFactory(String key, ArgumentParserElementFactory value);
    ArgumentParserElementFactory getFactory(String key);
    void parse( List<String> arguments );
}