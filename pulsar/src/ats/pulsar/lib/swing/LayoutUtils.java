package ats.pulsar.lib.swing;

import java.awt.GridBagConstraints;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;

import ats.pulsar.lib.swing.SchemeUtils;
import gnu.lists.Pair;

public class LayoutUtils {
	
	public static int str2gbc( Object o ) {
		if ( o == null ) {
			throw new NullPointerException( "str2gbc cannot convert null" );
		}

		String s = SchemeUtils.toString( o );
		s = s.replaceAll( "-" , "_" ) .toUpperCase();
		
		switch ( s ) {
			case "RELATIVE"                : return -1;
			case "REMAINDER"               : return 0;
			case "NONE"                    : return 0;
			case "BOTH"                    : return 1;
			case "HORIZONTAL"              : return 2;
			case "VERTICAL"                : return 3;
			case "CENTER"                  : return 10;
			case "NORTH"                   : return 11;
			case "NORTHEAST"               : return 12;
			case "EAST"                    : return 13;
			case "SOUTHEAST"               : return 14;
			case "SOUTH"                   : return 15;
			case "SOUTHWEST"               : return 16;
			case "WEST"                    : return 17;
			case "NORTHWEST"               : return 18;
			case "PAGE_START"              : return 19;
			case "PAGE_END"                : return 20;
			case "LINE_START"              : return 21;
			case "LINE_END"                : return 22;
			case "FIRST_LINE_START"        : return 23;
			case "FIRST_LINE_END"          : return 24;
			case "LAST_LINE_START"         : return 25;
			case "LAST_LINE_END"           : return 26;
			case "BASELINE"                : return 0x100;
			case "BASELINE_LEADING"        : return 0x200;
			case "BASELINE_TRAILING"       : return 0x300;
			case "ABOVE_BASELINE"          : return 0x400;
			case "ABOVE_BASELINE_LEADING"  : return 0x500;
			case "ABOVE_BASELINE_TRAILING" : return 0x600;
			case "BELOW_BASELINE"          : return 0x700;
			case "BELOW_BASELINE_LEADING"  : return 0x800;
			case "BELOW_BASELINE_TRAILING" : return 0x900;
			default : throw new RuntimeException( "unknown constraint : " + o.toString()  );
		}
	}
	
	public static Object map2constraint( Object object ) {
		Map<String, Object> map = SchemeUtils.list2map((Pair)object, null );
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
						SchemeUtils.list2map((Pair)entry.getValue(), (Object type)->{
							return (idx)->{
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
							};	
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
