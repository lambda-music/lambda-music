package pulsar;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import gnu.lists.LList;
import gnu.mapping.Procedure;
import gnu.mapping.Symbol;
import metro.Metro;
import metro.MetroPort;
import metro.MetroSyncType;
import pulsar.lib.scheme.SchemeUtils;

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
    
    /*
     * (Fri, 01 Nov 2019 06:02:11 +0900) 
     * See DEFAULT_BAR_LENGTH ;
     * now the bar length default to 1.0d. 
     * Hopefully this eases the learning Pulsar for beginners.
     */

    public static final Symbol ID_ID            = s( "id" );
    public static final Symbol ID_TAGS          = s( "tags" );
    public static final Symbol ID_SYNC_TYPE     = s( "syty" );
    public static final Symbol ID_SYNC_TRACK_ID = s( "syid" );
    public static final Symbol ID_SYNC_OFFSET   = s( "syof" );
    

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
            return SchemeUtils.toInteger( value );
        }
    };
    static final NoteListValueGenerator<Integer> DEFAULT_VALUE_INTEGER_0  = new NoteListValueGenerator.Default<Integer>( 0 ) ;
    static final NoteListValueGenerator<Integer> DEFAULT_VALUE_INTEGER_63 = new NoteListValueGenerator.Default<Integer>( 63 ) ;
    static final NoteListValueGenerator<Integer> DEFAULT_VALUE_INTEGER_60 = new NoteListValueGenerator.Default<Integer>( 60 ) ;
    static final NoteListValueConverter<Double> S2J_DOUBLE = new NoteListValueConverter<Double>() {
        @Override
        public Double convert(Object value) {
            return SchemeUtils.toDouble( value );
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

    static final NoteListValueGenerator<List> DEFAULT_VALUE_EMPTY_LIST = new NoteListValueGenerator<List>() {
        final List<Object> el = Collections.emptyList();
        @Override
        public List generate() {
            return el;
        }
    };    
    
    static boolean readMapEnabled( NoteListMap map ) {
        return map.get( ID_ENABLED, S2J_BOOLEAN, DEFAULT_VALUE_TRUE );
    }
    static int readMapChannel( NoteListMap map ) {
        return map.get( ID_CHANNEL, S2J_INTEGER, DEFAULT_VALUE_INTEGER_0 );
    }
    static double readMapOffset( NoteListMap map ) {
        return map.get( ID_OFFSET, S2J_DOUBLE, DEFAULT_VALUE_DOUBLE_0 );
    }
    static int readMapNote( NoteListMap map ) {
        // in some code it used to default to 63 now it defaults to 60;
        // https://en.scratch-wiki.info/wiki/MIDI_Notes
        // Note that C4 is 60.
        return map.get( ID_NOTE, S2J_INTEGER, DEFAULT_VALUE_INTEGER_60 );
    }
    static double readMapVelocity( NoteListMap map ) {
        // This used to default to 1.0d now it defaults to 0.5d.
        return map.get( ID_VELOCITY, S2J_DOUBLE, DEFAULT_VALUE_DOUBLE_5_10 );
    }
    static Integer readMapKey(NoteListMap map) {
        return map.get( ID_KEY,   S2J_INTEGER, DEFAULT_VALUE_INTEGER_0 );
    }
    static double readMapDoubleValue( NoteListMap map ) {
        return map.get( ID_VALUE, S2J_DOUBLE, DEFAULT_VALUE_DOUBLE_0 );
    }
    static double readMapDoubleValueBarLength( NoteListMap map ) {
        return map.get( ID_VALUE, S2J_DOUBLE, DEFAULT_VALUE_DOUBLE_BAR_LENGTH );
    }
    static int readMapIntegerValueDefault0( NoteListMap map ) {
        return map.get( ID_VALUE, S2J_INTEGER, DEFAULT_VALUE_INTEGER_0 );
    }
    static boolean readMapBooleanValueDefaultFalse( NoteListMap map ) {
        return map.get( ID_VALUE, S2J_BOOLEAN, DEFAULT_VALUE_FALSE );
    }
    static MetroPort readMapPort(Metro metro, NoteListMap map) {
        return ((Pulsar)metro).getPort( ID_PORT, map );
    }
    static double readMapNoteLength( NoteListMap map ) {
        return map.get( ID_LENGTH, S2J_DOUBLE, DEFAULT_VALUE_DOUBLE_NOTE_LENGTH );
    }
    static Procedure readMapProcedure( NoteListMap map ) {
        return map.get( ID_PROCEDURE, CAST_PROCEDURE, (NoteListValueGenerator<Procedure>)NoteListValueGenerator.NULL );
    }
    static List readMapList( Symbol id, NoteListMap map ) {
        return map.get( id, S2J_LIST, DEFAULT_VALUE_EMPTY_LIST );
    }
    
    static final NoteListValueGenerator<MetroSyncType> DEFAULT_VALUE_SYNC_TYPE = 
            new NoteListValueGenerator.Default<MetroSyncType>( MetroSyncType.SERIAL );
    static final NoteListValueConverter<MetroSyncType> S2J_SYNC_TYPE = new NoteListValueConverter<MetroSyncType>() {
        @Override
        public MetroSyncType convert(Object value) {
            return MetroSyncType.toSyncType( SchemeUtils.schemeSymbolToJavaString( SchemeUtils.schemeNullCheck( value ) ) );
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
            return SchemeUtils.schemeSymbol( createTempNewId() );
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
