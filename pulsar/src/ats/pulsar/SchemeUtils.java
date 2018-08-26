package ats.pulsar;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

import gnu.lists.AbstractSequence;
import gnu.lists.IString;
import gnu.lists.Pair;
import gnu.mapping.Symbol;
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
							throw new RuntimeException( "Error : cannot omit idx2nameGenerator." );
						} else {
							idx2name = idx2nameGenerator.apply( value );
						}
					}
				} else {
					String key = idx2name.apply( index );
					Object value = e;
					map.put( key , value );
					
					if ( "type".equals( key ) ) {
						idx2name = idx2nameGenerator.apply( value );
					}
				}
			}
		}
		return map;
	}

	public static String anyToString( Object schemeVal ) {
		if ( schemeVal instanceof IString ) {
			return ((IString)schemeVal).toString();
		} else if ( schemeVal instanceof Symbol ) {
			return ((Symbol)schemeVal).getName();
		} else {
			return ((Quantity) schemeVal).toString();
		}
	}
	
	public static String toString( Object schemeVal ) {
		return ((IString)schemeVal).toString();
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
	public static String className(Object object) {
		return object == null ? "null" : object.getClass().toString(); 
	}

	public static <T> List<T> convList(Pair p, Function<Object,T> conv ) {
		ArrayList<T> list=  new ArrayList<>( p.size() );
		for ( Object o : p )
			list.add( conv.apply( o ) );
		return list;
	}
}
