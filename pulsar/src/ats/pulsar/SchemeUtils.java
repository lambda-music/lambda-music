package ats.pulsar;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.function.Function;

import gnu.lists.AbstractSequence;
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
					map.put( idx2name.apply( counter ) , e );
					counter++;
				}
			}
		}
		return map;
	}

	static double toDouble( Object schemeVal ) {
		return ((Quantity) schemeVal).doubleValue();
	}
	static int toInteger( Object schemeVal ) {
		return ((Quantity) schemeVal).intValue();
	}

}
