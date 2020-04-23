package pulsar;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import gnu.lists.EmptyList;
import gnu.lists.IString;
import gnu.lists.LList;
import gnu.lists.Pair;
import gnu.mapping.Procedure;
import gnu.mapping.Symbol;
import gnu.math.IntNum;
import gnu.math.RealNum;
import lamu.lib.kawautils.SchemeValues;
import metro.Metro;
import metro.MetroPort;
import metro.MetroSyncType;

/**
 * (Fri, 01 Nov 2019 06:07:56 +0900)
 * 
 * Dependency for Pulsar from NoteListParsers
 * 
 * Formerly PulsarNoteListParsers are designed to be independent from Pulsar reference;
 * it only depends on Metro reference. Thought, it degraded to depend on Pulsar gradually.
 * There are some dependencies which is very difficult to trace.
 * 
 * {@link Pulsar#getPort(Symbol, NoteListMap)} is one of them. And another dependency is 
 * {@link pulsar.PulsarSpecialNoteListParsers#PARSER_EXEC}
 * 
 *  (Fri, 01 Nov 2019 11:15:31 +0900)
 *  Dependency from NoteListMap is resolved.
 *  
 */
public class NoteListCommon {
    static Symbol s( String name ) {
        //      SchemeUtils.schemeSymbol
        return Symbol.valueOf( name );
    }
    
    
    /**
     * if the `enab` field of a note is set to #f, the note will not be played.
     */
    public static final Symbol ID_ENABLED   = s( "enab" );
    public static final Symbol ID_CHANNEL   = s( "chan" );
    public static final Symbol ID_PORT      = s( "port" );
    public static final Symbol ID_PROCEDURE = s( "proc" );
//  public static final Symbol ID_ID        = s( "id" );
    public static final Symbol ID_LONG      = s( "XXX" ); // TODO
    
    public static final double DEFAULT_NOTE_LENGTH = 0.0025d;
    public static final double DEFAULT_BAR_LENGTH = 1.0d;

    public static final Symbol ID_TYPE  = s( "type" );
    public static final Symbol ID_NULL  = s( "" );
    
    /*
     * (Fri, 01 Nov 2019 06:02:11 +0900) 
     * See DEFAULT_BAR_LENGTH ;
     * now the bar length default to 1.0d. 
     * Hopefully this eases the learning Pulsar for beginners.
     */

    public static final Symbol ID_ID            = s( "id"   );
    public static final Symbol ID_TAGS          = s( "tags" );
    public static final Symbol ID_SYNC_TYPE     = s( "styp" );
    public static final Symbol ID_SYNC_TRACK_ID = s( "stra" );
    public static final Symbol ID_SYNC_OFFSET   = s( "soff" );
    

    /*
     * TODO 
     * ID_LENGTH should be renamed to a name which is not misleading.  
     */
    public static final Symbol ID_LENGTH    = s( "len" );
    public static final Symbol ID_VELOCITY  = s( "velo");
    public static final Symbol ID_NOTE      = s( "note");
    public static final Symbol ID_OFFSET    = s( "pos" );
    public static final Symbol ID_KEY       = s( "key" );
//  public static final Symbol ID_MIN       = s( "min" );
//  public static final Symbol ID_MAX       = s( "max" );
    public static final Symbol ID_VALUE     = s( "val" );

    public static final Symbol ID_MESSAGE   = s( "msg" );
    
    public static final NoteListValueConverter THRU = NoteListValueConverter.THRU;
    public static final NoteListValueGenerator NULL = NoteListValueGenerator.NULL;

    public static final NoteListValueConverter<Symbol> SYMBOL_THRU = NoteListValueConverter.THRU;
    public static final NoteListValueGenerator<Symbol> SYMBOL_NULL = (NoteListValueGenerator<Symbol>) NoteListValueGenerator.NULL;

    static final NoteListValueGenerator<Symbol> DEFAULT_VALUE_TYPE = 
            new NoteListValueGenerator.Default<Symbol>( SchemeValues.toSchemeSymbol( "note" ) ) ;

    static final NoteListValueConverter<Boolean> S2J_BOOLEAN = new NoteListValueConverter<Boolean>() {
        @Override
        public Boolean convert(Object value) {
            return (Boolean)value; 
        }
    };
    static final NoteListValueConverter<Boolean> J2S_BOOLEAN = S2J_BOOLEAN;
    static final NoteListValueGenerator<Boolean> DEFAULT_VALUE_TRUE  = new NoteListValueGenerator.Default( Boolean.TRUE );
    static final NoteListValueGenerator<Boolean> DEFAULT_VALUE_FALSE = new NoteListValueGenerator.Default( Boolean.FALSE );
    
    static final NoteListValueConverter<Integer> S2J_INTEGER = new NoteListValueConverter<Integer>() {
        @Override
        public Integer convert(Object value) {
            return SchemeValues.toInteger( value );
        }
    };
    static final NoteListValueConverter<IntNum> J2S_INTEGER = new NoteListValueConverter<IntNum>() {
        @Override
        public IntNum convert(Object value) {
            return SchemeValues.toSchemeNumber( (int)value );
        }
    };

    static final NoteListValueGenerator<Integer> DEFAULT_VALUE_INTEGER_0  = new NoteListValueGenerator.Default<Integer>( 0 ) ;

    /**
     * in some code it used to default to 63 now it defaults to 60;
     * https://en.scratch-wiki.info/wiki/MIDI_Notes
     * Note that C4 is 60.
     */
    static final NoteListValueGenerator<Integer> DEFAULT_VALUE_NOTE = new NoteListValueGenerator.Default<Integer>( 60 ) ;
    
    static final NoteListValueConverter<Double> S2J_DOUBLE = new NoteListValueConverter<Double>() {
        @Override
        public Double convert(Object value) {
            return SchemeValues.toDouble( value );
        }
    };
    static final NoteListValueConverter<RealNum> J2S_DOUBLE = new NoteListValueConverter<RealNum>() {
        @Override
        public RealNum convert(Object value) {
            return SchemeValues.toSchemeNumber( (double)value );
        }
    };

    static final NoteListValueGenerator<Double> DEFAULT_VALUE_DOUBLE_0 = new NoteListValueGenerator.Default<Double>( 0.0d ) ;
    static final NoteListValueGenerator<Double> DEFAULT_VALUE_DOUBLE_NOTE_LENGTH = new NoteListValueGenerator.Default<Double>( DEFAULT_NOTE_LENGTH ) ;
    static final NoteListValueGenerator<Double> DEFAULT_VALUE_DOUBLE_BAR_LENGTH  = new NoteListValueGenerator.Default<Double>( DEFAULT_BAR_LENGTH ) ;
    static final NoteListValueGenerator<Double> DEFAULT_VALUE_DOUBLE_MINUS_1 = new NoteListValueGenerator.Default<Double>( -1.0d ) ;
    static final NoteListValueGenerator<Double> DEFAULT_VALUE_DOUBLE_5_10 = new NoteListValueGenerator.Default<Double>( 0.5d ) ;
    static final NoteListValueConverter CAST_PROCEDURE = new NoteListValueConverter<Procedure>() {
        @Override
        public Procedure convert(Object value) {
            return (Procedure)value;
        }
    };
    // ??? (Fri, 01 Nov 2019 07:11:27 +0900)
    static final NoteListValueConverter<List> S2J_DUP_LIST = new NoteListValueConverter<List>() {
        @Override
        public List convert(Object value) {
            return new ArrayList( ((LList)value));
        }
    };
    // ??? (Fri, 01 Nov 2019 07:11:27 +0900)
    static final NoteListValueConverter<List> S2J_LIST = new NoteListValueConverter<List>() {
        @Override
        public List convert(Object value) {
            if ( value instanceof List ) { 
                return (List)value;
            } else if ( value instanceof Collection ) { 
                    return new ArrayList( (Collection)value );
            } else {
                return new ArrayList( Arrays.asList( value ) );
            }
        }
    };
    static final NoteListValueConverter<LList> J2S_LIST = new NoteListValueConverter<LList>() {
        @Override
        public LList convert(Object value) {
            if ( value instanceof List ) { 
                return LList.makeList( (List)value );
            } else if ( value instanceof Collection ) { 
                return LList.makeList( new ArrayList<>( (Collection)value  ) );
            } else {
                return Pair.make( value, EmptyList.emptyList ) ;
            }
        }
    };

    static final NoteListValueGenerator<List> DEFAULT_VALUE_EMPTY_LIST = new NoteListValueGenerator<List>() {
        final List<Object> el = Collections.emptyList();
        @Override
        public List generate() {
            return el;
        }
    };

    
    static final NoteListValueConverter<IString> J2S_STRING = new NoteListValueConverter<IString>() {
        @Override
        public IString convert(Object value) {
            return SchemeValues.toSchemeString( value.toString() );
        }
    };
    static final NoteListValueConverter<String> S2J_STRING = new NoteListValueConverter<String>() {
        @Override
        public String convert(Object value) {
            return SchemeValues.toString( value );
        }
    };


    ///////////////////////////////////////////////////////////////////////////
    // ** SPECIAL** the converter and the default generator for MetroPort
    ///////////////////////////////////////////////////////////////////////////
//    static final NoteListValueConverter<MetroPort> S2J_PORT = createS2JPort();
    private static final NoteListValueConverter<Object> J2S_PORT = new NoteListValueConverter<Object>() {
        @Override
        public Object convert( Object o ) {
            return ((MetroPort)o).getName();
        }
    };
    static NoteListValueConverter<Object> createJ2SPort() {
        return J2S_PORT;
    }

    // Modified createS2JPort() (Sat, 18 Apr 2020 18:04:36 +0900)
    static final NoteListValueConverter<MetroPort> S2J_PORT = new NoteListValueConverter<MetroPort>() {
        @Override
        public MetroPort convert(Object o) {
            if ( o instanceof MetroPort ) {
                return (MetroPort)o;
            } else if ( o instanceof Number ) {
                int i = ((Number)o).intValue();
                Metro metro = Pulsar.getCurrentMetro();
                List<MetroPort> list = metro.getOutputPorts();
                if ( i<0  ||  list.size() <= i )
                    throw new IllegalStateException( i + " is not proper. list size=" + list.size() );
                return list.get(i);
            } else {
                Metro metro = Pulsar.getCurrentMetro();
                List<MetroPort> portList = metro.searchOutputPort( o );
                if ( portList.isEmpty() ) {
                    throw new IllegalArgumentException( "'" + o + "' does not exists" );
                }
                return portList.get( 0 );
            }
        }
    };

    // Modified createDefaultValuePort() (Sat, 18 Apr 2020 18:04:36 +0900)
    static final NoteListValueGenerator<MetroPort> DEFAULT_VALUE_PORT = new NoteListValueGenerator<MetroPort>() {
        @Override
        public MetroPort generate() {
            Metro metro = Pulsar.getCurrentMetro();
            List<MetroPort> list = metro.getOutputPorts();
            if ( list.isEmpty() )
                throw new IllegalStateException();
            return list.get(0);
        }
    };

    static final LList list( Pair ... pairs ) {
        return LList.makeList( Arrays.asList( pairs ) );
    }

    static Symbol readMapType( NoteListMap map ) {
        return map.get( ID_TYPE, SYMBOL_THRU, DEFAULT_VALUE_TYPE );
    }
    static Pair writeMapType( Symbol value ) {
        return Pair.make( ID_TYPE, SYMBOL_THRU.convert( value ) );
    }
    
    static boolean readMapEnabled( NoteListMap map ) {
        return map.get( ID_ENABLED, S2J_BOOLEAN, DEFAULT_VALUE_TRUE );
    }
    static Pair writeMapEnabled( boolean value ) {
        return Pair.make( ID_ENABLED, J2S_BOOLEAN.convert( value ) );
    }
    
    static int readMapChannel( NoteListMap map ) {
        return map.get( ID_CHANNEL, S2J_INTEGER, DEFAULT_VALUE_INTEGER_0 );
    }
    static Pair writeMapChannel( int value ) {
        return Pair.make( ID_CHANNEL, J2S_INTEGER.convert( value ));
    }

    
    static double readMapOffset( NoteListMap map ) {
        return map.get( ID_OFFSET, S2J_DOUBLE, DEFAULT_VALUE_DOUBLE_0 );
    }
    static Pair writeMapOffset( double value) {
        return Pair.make( ID_OFFSET, J2S_DOUBLE.convert( value ) );
    }
    static int readMapNote( NoteListMap map ) {
        return map.get( ID_NOTE, S2J_INTEGER, DEFAULT_VALUE_NOTE );
    }
    static Pair writeMapNote( int value ) {
        return Pair.make( ID_NOTE, J2S_INTEGER.convert( value ) );
    }
    static double readMapVelocity( NoteListMap map ) {
        // This used to default to 1.0d now it defaults to 0.5d.
        return map.get( ID_VELOCITY, S2J_DOUBLE, DEFAULT_VALUE_DOUBLE_5_10 );
    }
    static Pair writeMapVelocity( double value ) {
        return Pair.make( ID_VELOCITY, J2S_DOUBLE.convert( value ));
    }
    static Integer readMapKey(NoteListMap map) {
        return map.get( ID_KEY,   S2J_INTEGER, DEFAULT_VALUE_INTEGER_0 );
    }
    static Pair writeMapKey(int value ) {
        return Pair.make( ID_KEY,   J2S_INTEGER.convert( value ) );
    }
    static double readMapDoubleValue( NoteListMap map ) {
        return map.get( ID_VALUE, S2J_DOUBLE, DEFAULT_VALUE_DOUBLE_0 );
    }
    static Pair writeMapDoubleValue( double value ) {
        return Pair.make( ID_VALUE, J2S_DOUBLE.convert( value ) );
    }
    static double readMapDoubleValueBarLength( NoteListMap map ) {
        return map.get( ID_VALUE, S2J_DOUBLE, DEFAULT_VALUE_DOUBLE_BAR_LENGTH );
    }
    static Pair writeMapDoubleValueBarLength( double value ) {
        return Pair.make( ID_VALUE, J2S_DOUBLE.convert( value ) );
    }
    static int readMapIntegerValueDefault0( NoteListMap map ) {
        return map.get( ID_VALUE, S2J_INTEGER, DEFAULT_VALUE_INTEGER_0 );
    }
    static Pair writeMapIntegerValueDefault0( int value ) {
        return Pair.make( ID_VALUE, J2S_INTEGER.convert( value ) );
    }
    static boolean readMapBooleanValueDefaultFalse( NoteListMap map ) {
        return map.get( ID_VALUE, S2J_BOOLEAN, DEFAULT_VALUE_FALSE );
    }
    static Pair writeMapBooleanValueDefaultFalse( boolean value ) {
        return Pair.make( ID_VALUE, J2S_BOOLEAN.convert( value ) );
    }
    /**
     * @see NoteListParser#parseAll(metro.Metro, metro.MetroTrack, Collection, metro.MetroBufferedMidiReceiver, boolean)
     */
    static MetroPort readMapPort( NoteListMap map ) {
        return map.get( ID_PORT, S2J_PORT, DEFAULT_VALUE_PORT );
    }
    static Pair writeMapPort( MetroPort value ) {
        return Pair.make( ID_PORT, createJ2SPort().convert( value ) );
    }
    static double readMapNoteLength( NoteListMap map ) {
        return map.get( ID_LENGTH, S2J_DOUBLE, DEFAULT_VALUE_DOUBLE_NOTE_LENGTH );
    }
    static Pair writeMapNoteLength( double value ) {
        return Pair.make( ID_LENGTH, J2S_DOUBLE.convert( value ) );
    }
    static Procedure readMapProcedure( NoteListMap map ) {
        return map.get( ID_PROCEDURE, CAST_PROCEDURE, (NoteListValueGenerator<Procedure>)NoteListValueGenerator.NULL );
    }
//    static Pair writeMapProcedure( Object value ) {
//        return Pair.make( ID_PROCEDURE, J2S_PROCEDURE.convert( value ) );
//    }
    static List readMapList( Symbol id, NoteListMap map ) {
        return map.get( id, S2J_LIST, DEFAULT_VALUE_EMPTY_LIST );
    }
    static Pair writeMapList( Symbol id, Object value ) {
        return Pair.make( id, J2S_LIST.convert( value ) );
    }
    static String readMapString( Symbol id, NoteListMap map ) {
        return map.get( id, S2J_STRING, NULL );
    }
    static Pair writeMapString( Symbol id, Object value ) {
        return Pair.make( id, J2S_STRING.convert( value ) );
    }
    
    static final NoteListValueGenerator<MetroSyncType> DEFAULT_VALUE_SYNC_TYPE = 
            new NoteListValueGenerator.Default<MetroSyncType>( MetroSyncType.SERIAL );
    static final NoteListValueConverter<MetroSyncType> S2J_SYNC_TYPE = new NoteListValueConverter<MetroSyncType>() {
        @Override
        public MetroSyncType convert(Object value) {
            return MetroSyncType.toSyncType( SchemeValues.toString(SchemeValues.schemeNullCheck( value )) );
        }
    };
    static final NoteListValueConverter<Symbol> J2S_SYNC_TYPE = new NoteListValueConverter<Symbol>() {
        @Override
        public Symbol convert(Object value) {
            return SchemeValues.toSchemeSymbol( ((MetroSyncType)value ).name().toLowerCase() );
        }
    };
    
    static MetroSyncType readMapSyncType(NoteListMap map) {
        return map.get( ID_SYNC_TYPE, S2J_SYNC_TYPE, DEFAULT_VALUE_SYNC_TYPE );
    }

    static final NoteListValueConverter<Procedure> S2J_PROCEDURE = new NoteListValueConverter<Procedure>() {
        @Override
        public Procedure convert(Object value) {
            return SchemeSequence.asProcedure( value );
        }
    };

//    static final NoteListValueConverter<Procedure> J2S_PROCEDURE = new NoteListValueConverter<Procedure>() {
//        @Override
//        public Procedure convert(Object value) {
//            return SchemeSequence.asProcedure( value );
//        }
//    };

    
    ////////////////////////////////////////////////////////////////////////////////////
    
    static final NoteListValueGenerator<Symbol> DEFAULT_VALUE_NEW_UNIQUE_SYMBOL = new NoteListValueGenerator<Symbol>() {
        int tempNewIdCounter = 0;
        Object getTempNewIdCounterLock() {
            return PulsarSpecialNoteListParsers.class;
        }
        String createTempNewId() {
            synchronized ( getTempNewIdCounterLock() ) {
                return "TEMPID-" + ( tempNewIdCounter++ );
            }
        }
        @Override
        public Symbol generate() {
            return SchemeValues.toSchemeSymbol( createTempNewId() );
        }
    };

    static Object readMapNewId(NoteListMap map) {
        return map.get( ID_ID , NoteListValueConverter.THRU, DEFAULT_VALUE_NEW_UNIQUE_SYMBOL );
    }
    ////////////////////////////////////////////////////////////////////////////////////

    static List readMapCollection( Symbol id, NoteListMap map ) {
        return map.get( id, S2J_LIST, DEFAULT_VALUE_EMPTY_LIST );
    }

}
