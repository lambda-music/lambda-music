package lamu.lib.kawautils;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.invoke.MethodHandles;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.logging.Level;
import java.util.stream.Collectors;

import gnu.kawa.slib.srfi1;
import gnu.lists.EmptyList;
import gnu.lists.IString;
import gnu.lists.LList;
import gnu.lists.Pair;
import gnu.mapping.Environment;
import gnu.mapping.Procedure;
import gnu.mapping.Symbol;
import gnu.mapping.Values;
import gnu.math.DFloNum;
import gnu.math.IntNum;
import gnu.math.Quantity;
import lamu.lib.kawautils.procedures.MultipleNamed;
import lamu.lib.logging.Logger;

public class SchemeValues {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg) { LOGGER.log(Level.INFO, msg); }
    static void logWarn(String msg) { LOGGER.log(Level.WARNING, msg); }
    
    
    public static final Object NO_RESULT = Values.empty;
    // ADDED (Wed, 22 Apr 2020 13:35:41 +0900)
    public static List<String> toStringList(List<?> list) {
        return list.stream().map( (v)->v==null ? null : v.toString() ).collect(Collectors.toList());
    }
    public static String anyToString( Object schemeVal ) {
        return toString(schemeVal);
    }
    public static String toString( Object schemeVal ) {
        return schemeVal == null ? null : schemeVal.toString();
    }
    
// OBSOLETE (Wed, 22 Apr 2020 16:15:08 +0900)
//    public static String toString( Object value ) {
//        if ( value == null ) 
//            return null;
//        else
//            return value.toString();
//    }
//    // If it is allowed to return null (in case such return type is Object), then
//    // return null. Otherwise, ( in case such return type is double) then throw an
//    // error. (Tue, 06 Aug 2019 14:41:05 +0900) 
//    public static String anyToString( Object schemeVal ) {
//        // modified (Wed, 31 Jul 2019 21:51:55 +0900)
//        // reverted (Sun, 04 Aug 2019 21:52:21 +0900)  
//        // return toString( value );
//        if ( schemeVal == null ) {
//            return null;
//        } else if ( schemeVal instanceof Symbol ) {
//            return fromSymbol(schemeVal);
//        } else {
//            return toString( schemeVal );
//        }
//    }
    
    /**
     * Return the argument value itself if the value is not a procedure; otherwise
     * this method calls the procedure and return the value with the result of the
     * procedure.
     * 
     * @param schemeVal
     *    any value.
     * @return
     *    the argument value unless it is a procedure.
     */
    public static Object resolveProcedure( Object schemeVal ) {
        Object result = schemeVal;
        if ( result instanceof Procedure ) {
            try {
                result = ((Procedure)result).apply0();
            } catch (Throwable e) {
                logError("", e);
            }
        }
        return result;
    }
    
    public static boolean isQuantity( Object schemeVal ) {
        return schemeVal instanceof Quantity;
    }
    public static double toDouble( Object schemeVal ) {
        return ((Quantity) schemeVal).doubleValue();
    }
    public static float toFloat( Object schemeVal ) {
        return ((Quantity) schemeVal).floatValue();
    }
    public static int toInteger( Object schemeVal ) {
        return ((Quantity) schemeVal).intValue();
    }
    public static long toLong( Object schemeVal ) {
        return ((Quantity) schemeVal).longValue();
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
    
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    
    /**
     * Treat everything as Java's //true// except 
     * Scheme's //#f// and Java's //null//. 
     * Note that scheme's //'()// is treated as Java's //true//.
     * Scheme does not have a counterpart for Java's null.
     */
    public static Boolean toBoolean( Object schemeVal ) {
        if ( schemeVal == null )
            return false;
        else if ( schemeVal instanceof Boolean)
            return (Boolean)schemeVal;
        else
            return Boolean.TRUE; 
    }

    public static <T> T schemeNullCheck( T object ) {
        if ( object == null ) {
            return null;
        } else if ( object instanceof EmptyList ) {
            return null;
        } else if ( Boolean.FALSE.equals( object ) ) {
            return null;
        } else {
            return object;
        }
    }
    public static Object javaNullCheck( Object object ) {
        if ( object == null ) {
            return false;
        } else {
            return object;
        }
    }

    
    
    // ADDED (Tue, 17 Mar 2020 23:58:15 +0900) >>>
    public static Object string2lisp( String input ) throws IOException {
        Object result;
        try (
            gnu.kawa.io.CharArrayInPort port = new gnu.kawa.io.CharArrayInPort( input ); 
            gnu.kawa.lispexpr.LispReader reader = new gnu.kawa.lispexpr.LispReader(port);
            )
        {
            result= reader.readObject();
        }
        return result;
    }
    
    public static String lisp2string( Object schemeValue ) {
        return gnu.kawa.functions.DisplayFormat.schemeWriteFormat.format( schemeValue );
    }
    
    static void test_lispt2string() throws Throwable {
        Object schemeValue = SchemeValues.string2lisp( "(( a . hello )\n( b . foo )\n;hello world\n( c . 4 )(d . 5)( e . \"world\") )\n" );
        System.out.println( cdr( assq( "b", schemeValue ) ) );
        String stringValue = SchemeValues.lisp2string( schemeValue );
        System.out.println( stringValue );; 
        System.out.println( stringValue.getClass() );; 
    }
    public static void main(String[] args) throws Throwable {
        test_lispt2string();
    }
    // ADDED (Tue, 17 Mar 2020 23:58:15 +0900) <<<
    public static byte[] readAll( InputStream in ) throws IOException {
        ByteArrayOutputStream result = new ByteArrayOutputStream();
        byte[] buf = new byte[1024];
        int length;
        while ((length = in.read(buf)) != -1) {
            result.write(buf, 0, length);
        }
        return result.toByteArray();
    }
    
    
    private static final boolean DEBUG_ENV = false;
    
    /**
     * This method is going to be deprecated.
     * 
     * @param env
     * @param value
     * @param names
     */
    public static final void defineVar( Environment env, Object value, Collection<String> names ) {
        if ( env == null )
            return;
        // env = Environment.getCurrent();
        if ( DEBUG_ENV )
            logInfo( "defineVar:" + env.getName() + ":" + names );

        for ( String name : names ) {
            env.define( toSchemeSymbol( name ), null, value );
        }
    }

    /**
     * 
     * @param env
     * @param value
     */
    public static final void defineLambda( Environment env, Procedure value ) {
        if ( value instanceof MultipleNamed ) {
            SchemeValues.defineVar( env, value, ((MultipleNamed)value).getNames() );
        } else {
            String[] names = { value.getName() };
            SchemeValues.defineVar( env, value, Arrays.asList( names ) );
        }
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    // srfi1
    /////////////////////////////////////////////////////////////////////////////////////////

    static final Procedure assq = (Procedure)srfi1.assq.get();
    static final Procedure car = (Procedure)srfi1.car.get();
    static final Procedure cdr = (Procedure)srfi1.cdr.get();
    static Object assq( String key, Object alist ) {
        try {
            return assq( gnu.mapping.Symbol.valueOf(key), alist );
        } catch (Throwable e) {
            throw new Error("internal error", e);
        }
    }
    public static Object assq( Object key, Object alist ) {
        try {
            return assq.apply2( key, alist );
        } catch (Throwable e) {
            throw new Error("internal error", e);
        }
    }
    public static Object car( Object object ) {
        try {
            return car.apply1(object);
        } catch (Throwable e) {
            throw new Error("internal error", e);
        }
    }
    public static Object cdr( Object object ) {
        try {
            return cdr.apply1(object);
        } catch (Throwable e) {
            throw new Error("internal error", e);
        }
    }

    public static void main3(String[] args) throws Throwable {
        LList alist = LList.makeList( Arrays.asList( 
            Pair.make( Symbol.valueOf( "a" ), Symbol.valueOf( "HELLO" ) ),
            Pair.make( Symbol.valueOf( "b" ), Symbol.valueOf( "WORLD" ) ) ) );
        System.out.println( SchemeValues.alistGet( Symbol.valueOf( "a" ), alist, "NOT FOUND" ) );   
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    // alist
    /////////////////////////////////////////////////////////////////////////////////////////
    public static Object alistGet( Object key, LList alist, Object defaultValue ) {
        try {
            Object value = assq.apply2( key, alist );
            if ( Boolean.FALSE == value ) {
                return defaultValue;
            } else {
                return ((Pair)value).getCdr();
            }
        } catch (Throwable e) {
            throw new RuntimeException(e);
        }
    }
    
    //    public static final void defineVar( Environment env, Object value, String ... names ) {
    //        defineVar( env, value, Arrays.asList( names ) );
    //    }

}
