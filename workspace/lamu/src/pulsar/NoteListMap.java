package pulsar;

import java.util.HashMap;
import java.util.Iterator;

import gnu.kawa.slib.srfi1;
import gnu.lists.LList;
import gnu.lists.Pair;
import gnu.mapping.Procedure;
import gnu.mapping.Symbol;

public abstract class NoteListMap {
    public abstract <T> T get( Symbol key, NoteListValueConverter<T> converter, NoteListValueGenerator<T> defaultValue );
    
    public static final NoteListMap createAlist( LList notation ) {
        return new NoteListSchemeAlistMap( notation );
    }
    private static class NoteListSchemeAlistMap extends NoteListMap {
        private static final Procedure assq = (Procedure) srfi1.assq.get();
        private static final <T> T alistGet( Object key, LList alist, NoteListValueConverter<T> converter, NoteListValueGenerator<T> defaultValue ) {
            try {
                Object value = assq.apply2( key, alist );
                if ( Boolean.FALSE == value ) {
                    return defaultValue.generate();
                } else {
                    return converter.convert( ((Pair)value).getCdr() );
                }
            } catch (Throwable e) {
                throw new RuntimeException(e);
            }
        }
        private LList alist;
        public NoteListSchemeAlistMap(LList alist) {
            this.alist = alist;
        }
        @Override
        public <T> T get(Symbol key, NoteListValueConverter<T> converter, NoteListValueGenerator<T> defaultValue) {
            return alistGet( key , alist, converter, defaultValue );
        }
    }
    
    public static final NoteListMap createHash( LList notation ) {
        return new NoteListHashMap( notation );
    }
    private static class NoteListHashMap extends NoteListMap {
        static HashMap<Symbol,Object> list2map( LList list ) {
            HashMap<Symbol,Object> result = new HashMap<Symbol,Object>();
            for ( Iterator<Object> i = list.iterator(); i.hasNext();  ) {
                Object e = i.next();
                if ( e instanceof Pair ) {
                    Pair p = (Pair)e;
                    Symbol key   = (Symbol)p.getCar();
                    Object value =         p.getCdr();
                    result.put( key, value );
                }
            }
            return result;
        }
        public NoteListHashMap( HashMap<Symbol,Object> map ) {
            this.map = map;
        }
        public NoteListHashMap(LList notation) {
            this( list2map( notation ) );
        }
        
        HashMap<Symbol,Object> map;
        @Override
        public <T> T get(Symbol key, NoteListValueConverter<T> converter, NoteListValueGenerator<T> defaultValue ) {
            return map.containsKey( key ) ? converter.convert( map.get( key ) ) : defaultValue.generate();
        }
        
    }
}
