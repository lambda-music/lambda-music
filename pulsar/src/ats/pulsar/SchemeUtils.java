package ats.pulsar;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

import gnu.lists.AbstractSequence;
import gnu.lists.EmptyList;
import gnu.lists.IString;
import gnu.lists.Pair;
import gnu.mapping.Symbol;
import gnu.math.DFloNum;
import gnu.math.IntNum;
import gnu.math.Quantity;

public class SchemeUtils {

	public static Map<String,Object> list2map( AbstractSequence<Object> list, 
			Function<Object,Function<Integer,String>> idx2nameGenerator ) 
	{
		HashMap<String,Object> map = new HashMap<String,Object>();
		int index=-1;

		Function<Integer,String> idx2name = (Integer idx)->{
			if ( idx == 0 ) {
				return "type";
			} else {
				throw new RuntimeException( "Error : cannot omit element-name." );
			}
		};
		
		{
			for ( Iterator<Object> i = list.iterator(); i.hasNext();  ) {
				Object e = i.next();
				index++;
	
				if ( e instanceof Pair ) {
					Pair p = (Pair) e;
					String key = p.getCar().toString();
					Object value = p.getCdr();
					map.put( key, value );
					
					if ( "type".equals( key ) ) {
						if ( idx2nameGenerator == null ) {
							idx2name = null;
						} else {
							idx2name = idx2nameGenerator.apply( value );
						}
					}
				} else {
					System.out.println( e.getClass().getName() );
					String key = idx2name.apply( index );
					Object value = e;
					map.put( key , value );
					
					if ( "type".equals( key ) ) {
						if ( idx2nameGenerator == null ) {
							idx2name = null;
						} else {
							idx2name = idx2nameGenerator.apply( value );
						}
					}
				}
			}
		}
		return map;
	}
	
	public static Symbol schemeSymbol( String string ) {
		return Symbol.valueOf( string );
	}

	public static String anyToString( Object schemeVal ) {
		if ( schemeVal instanceof IString ) {
			return ((IString)schemeVal).toString();
		} else if ( schemeVal instanceof Symbol ) {
			return ((Symbol)schemeVal).getName();
		} else if ( schemeVal instanceof String ) {
			return (String) schemeVal;
		} else {
			return ((Quantity) schemeVal).toString();
		}
	}
	
	public static Boolean toBoolean( Object schemeVal ) {
		if ( schemeVal instanceof Boolean)
			return (Boolean)schemeVal;
		else
			return Boolean.TRUE; // treat everything as #t except #f.
	}
	public static String toString( Object schemeVal ) {
		return schemeVal.toString();
//		if ( schemeVal instanceof String )
//			return (String) schemeVal;
//		else
//			return ((IString)schemeVal).toString();
	}
	public static String symbolToString( Object schemeVal ) {
		return ((Symbol)schemeVal).getName();
	}
	public static double toDouble( Object schemeVal ) {
		return ((Quantity) schemeVal).doubleValue();
	}
	public static int toInteger( Object schemeVal ) {
		return ((Quantity) schemeVal).intValue();
	}
	public static long toLong( Object schemeVal ) {
		return ((Quantity) schemeVal).longValue();
	}
	public static String className(Object object) {
		return object == null ? "null" : object.getClass().toString(); 
	}

	@Deprecated
	public static <T> List<T> convList(Pair p, Function<Object,T> conv ) {
		ArrayList<T> list=  new ArrayList<>( p.size() );
		for ( Object o : p )
			list.add( conv.apply( o ) );
		return list;
	}
	
	public static <P,T> Collection<T> convertList(Collection<P> list, Function<P,T> f) {
		return list.stream().map(f).collect(Collectors.toList());
	}
	
	public static IString toSchemeString( String value ) {
		return IString.valueOf( value );
	}

	public static DFloNum toSchemeNumber(double value) {
		return DFloNum.valueOf(value);
	}
	public static IntNum toSchemeNumber(int value) {
		return IntNum.valueOf(value);
	}

	public static Symbol toSchemeSymbol(String value) {
		return Symbol.valueOf(value);
	}
	
	public static <T> T schemeNullToJavaNull( T object ) {
		if ( object instanceof EmptyList ) {
			return null;
		} else {
			return object;
		}
	}
	public static Object javaNullToSchemeNull( Object object ) {
		if ( object == null )
			return EmptyList.emptyList;
		else
			return object;
	}
	
	public static Pair acons( String key, Object value ) {
		return new Pair( toSchemeSymbol( key ) , value );
	}

}
