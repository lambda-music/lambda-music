package lamu.lib.kawautils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import gnu.expr.Keyword;

public class SchemeNamedArgs {
    /**
     * This parses arguments which form is in named argument format such as <code>(proc name: 'hello value: 100 )</code> 
     * @param args
     * @param namedArgs
     * @param numberedArgs
     */
    public static void parseArguments( Object[] args, Map<String,Object> namedArgs, List<Object> numberedArgs ) {
        for ( Iterator<Object> i=Arrays.asList( args ).iterator(); i.hasNext();  ) {
            Object o = i.next();
            if ( Keyword.isKeyword( o ) ) {
                 namedArgs.put(((Keyword)o).getName(), i.hasNext() ? i.next() : null );
            } else {
                numberedArgs.add(o);
            }
        }
    }

    private static class ConstArgsRef<T> implements SchemeNamedArgsRef<T> {
        private final T value;
        ConstArgsRef(T value) {
            super();
            this.value = value;
        }
        @Override
        public T getValue(Map<String, Object> namedArgs, List<Object> numberedArgs) {
            return value;
        }
    }
    private static SchemeNamedArgsRef NULL_VALUE_REFERENCE = new ConstArgsRef(null);
    public static <T> SchemeNamedArgsRef<T> constantRef( T value ) {
        return new ConstArgsRef<T>(value);
    }
    public static <T> SchemeNamedArgsRef<T> nullRef() {
        return NULL_VALUE_REFERENCE;
    }

    private static class NamedNumberedArgsRef<T> implements SchemeNamedArgsRef<T> {
        private final String name;
        private final int number;
        private final SchemeNamedArgsConv<T> converter;
        private final SchemeNamedArgsRef<T> defaultRef;
        NamedNumberedArgsRef(String key, int number, SchemeNamedArgsConv<T> converter, SchemeNamedArgsRef<T> defaultValue ) {
            this.name = key;
            this.number = number;
            this.converter = converter;
            this.defaultRef = defaultValue;
        }
        @Override
        public T getValue(Map<String, Object> namedArgs, List<Object> numberedArgs) {
            if ( (name != null) && namedArgs.containsKey(name) ) {
                return converter.convert( namedArgs.get(name) );
            } else if ( (0<=number) && number < numberedArgs.size() ) {
                return converter.convert( numberedArgs.get(number) );
            } else {
                return defaultRef.getValue(namedArgs, numberedArgs);
            }
        }
    }

    public static <T> SchemeNamedArgsRef<T> namedRef( String key, SchemeNamedArgsConv<T> converter, SchemeNamedArgsRef<T> defaultValue ) {
        return new NamedNumberedArgsRef<T>(key, -1, converter, defaultValue);
    }
    public static <T> SchemeNamedArgsRef<T> numberedRef( int index, SchemeNamedArgsConv<T> converter, SchemeNamedArgsRef<T> defaultValue ) {
        return new NamedNumberedArgsRef<T>(null, index, converter, defaultValue);
    }
    public static <T> SchemeNamedArgsRef<T> namedNumberedRef( String name, int number, SchemeNamedArgsConv<T> converter, SchemeNamedArgsRef<T> defaultRef ) {
        return new NamedNumberedArgsRef<T>(name, number, converter, defaultRef);
    }

    private final Map<String,Object> namedArgs;
    private final List<Object> numberedArgs;
    public SchemeNamedArgs( Object[] args ) {
        this.namedArgs = new HashMap<String, Object>();
        this.numberedArgs = new ArrayList<Object>();
        parseArguments( args, namedArgs, numberedArgs );
    }
    public Map<String, Object> getNamedArgs() {
        return namedArgs;
    }
    public List<Object> getNumberedArgs() {
        return numberedArgs;
    }
    public <T> T getValue( SchemeNamedArgsRef<T> reference ) {
        return reference.getValue(namedArgs, numberedArgs);
    }

}
