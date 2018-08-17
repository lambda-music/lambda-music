package ats.pulsar;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.function.Function;

import gnu.lists.AbstractSequence;
import gnu.lists.IString;
import gnu.lists.Pair;
import gnu.math.Quantity;

public class SchemeUtils {

	public static Map<String,Object> list2map( AbstractSequence<Object> list, Function<Integer,String> idx2name ) {
		HashMap<String,Object> map = new HashMap<String,Object>();
		int counter= 0;
		{
			for ( Iterator<Object> i = list.iterator(); i.hasNext();  ) {
				Object e = i.next();
				if ( e instanceof Pair ) {
					Pair p = (Pair) e;
					map.put( p.getCar().toString(), p.getCdr() );
				} else {
					if ( idx2name == null ) {
						throw new RuntimeException( "Error : cannot omit element-name." );
					}
					map.put( idx2name.apply( counter ) , e );
					counter++;
				}
			}
		}
		return map;
	}
	public static String toString( Object schemeVal ) {
		return ((IString)schemeVal).toString();
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
}
