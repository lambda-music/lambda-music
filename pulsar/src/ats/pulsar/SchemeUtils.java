package ats.pulsar;

import java.awt.GridBagConstraints;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
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

	
	public static Object map2constraint( Object object ) {
		Map<String, Object> map = list2map((Pair)object, null );
		GridBagConstraints gbc = new GridBagConstraints();
		
		for ( Iterator<Entry<String,Object>> i=map.entrySet().iterator(); i.hasNext();   ) {
			Entry<String, Object> entry = i.next();
			switch ( entry.getKey() ) {
				case "gridx" :
					gbc.gridx = SchemeUtils.toInteger( entry.getValue() );
					break;
				case "gridy" :
					gbc.gridy = SchemeUtils.toInteger( entry.getValue() );
					break;
				case "gridwidth" :
					gbc.gridwidth = SchemeUtils.toInteger( entry.getValue() );
					break;
				case "gridheight" :
					gbc.gridheight = SchemeUtils.toInteger( entry.getValue() );
					break;
				case "weightx" :
					gbc.weightx = SchemeUtils.toDouble( entry.getValue() );
					break;
				case "weighty" :
					gbc.weighty = SchemeUtils.toDouble( entry.getValue() );
					break;
				case "anchor" :
					gbc.anchor = SchemeUtils.toInteger( entry.getValue() );
					break;
				case "fill" :
					gbc.fill = SchemeUtils.toInteger( entry.getValue() );
					break;
				case "insets" :
					Map<String,Object> imap = 
						SchemeUtils.list2map((Pair)entry.getValue(), (idx)->{
							switch (idx) {
								case 0 :
									return "top";
								case 1 :
									return "left";
								case 2 :
									return "bottom";
								case 3 :
									return "right";
								default :
									throw new RuntimeException( "Error : undefined index of array." );
							}
					});

					if ( map.containsKey( "top" ) )
						gbc.insets.top = SchemeUtils.toInteger( imap.get( "top" ) );
					if ( map.containsKey( "left" ) )
						gbc.insets.left = SchemeUtils.toInteger( imap.get( "left" ) );
					if ( map.containsKey( "bottom" ) )
						gbc.insets.bottom = SchemeUtils.toInteger( imap.get( "bottom" ) );
					if ( map.containsKey( "right" ) )
						gbc.insets.right = SchemeUtils.toInteger( imap.get( "right" ) );
					
					break;
				case "ipadx" :
					gbc.ipadx = SchemeUtils.toInteger( entry.getValue() );
					break;
				case "ipady" :
					gbc.ipady = SchemeUtils.toInteger( entry.getValue() );
					break;
					
			}
		}
		return gbc;
	}
}
