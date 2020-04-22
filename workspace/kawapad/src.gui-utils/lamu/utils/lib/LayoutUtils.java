package lamu.utils.lib;

import java.awt.GridBagConstraints;
import java.lang.invoke.MethodHandles;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.function.Function;
import java.util.logging.Level;

import gnu.lists.AbstractSequence;
import gnu.lists.Pair;
import lamu.lib.kawautils.SchemeValues;
import lamu.lib.log.Logger;

public class LayoutUtils {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg) { LOGGER.log(Level.INFO, msg); }
    static void logWarn(String msg) { LOGGER.log(Level.WARNING, msg); }
    private static final boolean DEBUG = false;

    public static int str2gbc( Object o ) {
        if ( o == null ) {
            throw new NullPointerException( "str2gbc cannot convert null" );
        }

        String s = SchemeValues.toString( o );
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
                    String key   = SchemeValues.toString( p.getCar() );
                    //              String key   = p.getCar().toString();   //XXX TODO
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
                    if (DEBUG)
                        logInfo( e.getClass().getName() );

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

    public static Object map2constraint( Object object ) {
        Map<String, Object> map = list2map((Pair)object, null );
        GridBagConstraints gbc = new GridBagConstraints();

        for ( Iterator<Entry<String,Object>> i=map.entrySet().iterator(); i.hasNext();   ) {
            Entry<String, Object> entry = i.next();
            switch ( entry.getKey() ) {
            case "gridx" :
                gbc.gridx = SchemeValues.toInteger( entry.getValue() );
                break;
            case "gridy" :
                gbc.gridy = SchemeValues.toInteger( entry.getValue() );
                break;
            case "gridwidth" :
                gbc.gridwidth = SchemeValues.toInteger( entry.getValue() );
                break;
            case "gridheight" :
                gbc.gridheight = SchemeValues.toInteger( entry.getValue() );
                break;
            case "weightx" :
                gbc.weightx = SchemeValues.toDouble( entry.getValue() );
                break;
            case "weighty" :
                gbc.weighty = SchemeValues.toDouble( entry.getValue() );
                break;
            case "anchor" :
                gbc.anchor = SchemeValues.toInteger( entry.getValue() );
                break;
            case "fill" :
                gbc.fill = SchemeValues.toInteger( entry.getValue() );
                break;
            case "insets" :
                Map<String,Object> imap = 
                list2map((Pair)entry.getValue(), (Object type)->{
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
                    gbc.insets.top = SchemeValues.toInteger( imap.get( "top" ) );
                if ( map.containsKey( "left" ) )
                    gbc.insets.left = SchemeValues.toInteger( imap.get( "left" ) );
                if ( map.containsKey( "bottom" ) )
                    gbc.insets.bottom = SchemeValues.toInteger( imap.get( "bottom" ) );
                if ( map.containsKey( "right" ) )
                    gbc.insets.right = SchemeValues.toInteger( imap.get( "right" ) );

                break;
            case "ipadx" :
                gbc.ipadx = SchemeValues.toInteger( entry.getValue() );
                break;
            case "ipady" :
                gbc.ipady = SchemeValues.toInteger( entry.getValue() );
                break;

            }
        }
        return gbc;
    }
}
