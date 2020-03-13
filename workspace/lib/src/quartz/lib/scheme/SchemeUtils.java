/*
 * SchemeUtils written by Atsushi Oka 
 * Copyright 2018 Atsushi Oka
 *
 * This file is part of Metro Musical Sequencing Framework. 
 * 
 * SchemeUtils is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * SchemeUtils is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with SchemeUtils.  If not, see <https://www.gnu.org/licenses/>.
 */

package quartz.lib.scheme;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import gnu.expr.Language;
import gnu.kawa.slib.srfi1;
import gnu.lists.AbstractSequence;
import gnu.lists.EmptyList;
import gnu.lists.IString;
import gnu.lists.LList;
import gnu.lists.Pair;
import gnu.mapping.Environment;
import gnu.mapping.LocationEnumeration;
import gnu.mapping.NamedLocation;
import gnu.mapping.Procedure;
import gnu.mapping.SimpleSymbol;
import gnu.mapping.Symbol;
import gnu.mapping.Values;
import gnu.math.DFloNum;
import gnu.math.IntNum;
import gnu.math.Quantity;
import kawa.standard.Scheme;
import quartz.lib.log.LamuLogger;
import quartz.lib.scheme.proc.MultipleNamed;

public class SchemeUtils {
    public static final Object NO_RESULT = Values.empty;
    
    static final Logger LOGGER = LamuLogger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) {
        LOGGER.log(Level.SEVERE, msg, e);
    }
    static void logInfo(String msg) {
        LOGGER.log(Level.INFO, msg);
    }
    static void logWarn(String msg) {
        LOGGER.log(Level.WARNING, msg);
    }
    private static final boolean DEBUG = false;

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
                    String key   = fromSymbol( p.getCar() );
//                  String key   = p.getCar().toString();   //XXX TODO
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

    public static Map<Symbol,Object> list2map2( LList list ) {
        HashMap<Symbol,Object> result = new HashMap<Symbol,Object>();
        for ( Iterator<Object> i = list.iterator(); i.hasNext();  ) {
            Object e = i.next();
            if ( e instanceof Pair ) {
                Pair p = (Pair)e;
                Symbol key   = (Symbol)p.getCar();
                Object value =         p.getCdr();
                result.put( key, value );
            }
        }
        return result;
    }
    
    public static LList ls(Object... vals) {
        LList result = LList.Empty;
        for (int i = vals.length;  --i >= 0; ) {
            if ( vals[i] instanceof Object[] ) {
                result = ls((Object[])vals[i] );
            } else if ( vals[i] instanceof Symbol ) {
                result = new Pair( (Symbol)vals[i], result );
            } else if ( vals[i] instanceof String ) {
                result = new Pair( toSchemeSymbol((String)vals[i]), result );
            } else {
                result = new Pair( vals[i], result );
            }
        }
        return result;
    }

    public static Object eval( LList llist ){
        return eval( null, llist );
    }
    public static Object eval( Environment env, LList llist ){
//      System.out.println(env);
//      System.out.println(llist);
        if ( env == null ) {
            Scheme scheme = new Scheme();
            Environment.setCurrent( scheme.getEnvironment() );
            Language.setCurrentLanguage(scheme);
            env = scheme.getEnvironment();
        }
        try {
            return (kawa.lib.scheme.eval.eval).apply2(llist, env);
        } catch (Throwable e) {
            throw new RuntimeException(e);
        }
    }
    static void testEval1() throws Throwable {
        Procedure evaler = 
                (Procedure)eval(
                    ls("lambda", 
                        ls("script" ),
                        ls("let", 
                            ls(ls("hello","script") ),
                            ls("display", "hello"),
                            ls("newline") )));

        evaler.apply1( "hello world!" );
    }
    
    static Procedure evalString;
    static {
        evalString = (Procedure)eval(
            ls("lambda", 
                ls("env", "script"),
                ls("call-with-input-string","script",
                    ls("lambda",ls("script-port"),
                        ls( "let",ls( ls("script-list", 
                                      ls("read", "script-port" ))),
                            ls("display", "script-list"),
                            ls("newline"),
                            ls("eval","script-list", "env" ))))));
    }
    public static Object eval( String scriptString ) {
        return eval( null, scriptString );
    }
    public static Object eval( Environment env, String scriptString ) {
        if ( env == null ) {
            Scheme scheme = new Scheme();
            Environment.setCurrent( scheme.getEnvironment() );
            Language.setCurrentLanguage(scheme);
            env = scheme.getEnvironment();
        }
        try {
            return evalString.apply2( env, IString.valueOf( scriptString ) );
        } catch (Throwable e) {
            throw new RuntimeException(e);
        }
    }
    public static void testEval2() {
        eval( "(display (+ 1 1 1 ) )(newline)" ); 
    }
//  public static void main(String[] args) throws Throwable {
//      testEval2();
//  }
    public static Pair symbols(String ... args) {
        return (Pair)Pair.makeList( javaStringListToSchemeSymbolList(Arrays.asList(args)) );
    }

    public static List<Symbol> javaStringListToSchemeSymbolList( List<String> stringList) { 
        return SchemeUtils.<String,Symbol>convertList( stringList, (o)->{
            return SchemeUtils.schemeSymbol( o );
        });
    }
    public static List<IString> javaStringListToSchemeStringList( List<String> stringList) { 
        return SchemeUtils.<String,IString>convertList( stringList, (o)->{
            return SchemeUtils.schemeString( o );
        });
    }
    
    private static IString schemeString(String o) {
        return IString.valueOf( o ) ;
    }
    /*
     * We changed the policy the way to treat null here;
     * now we are ignoring null value. (Sun, 04 Aug 2019 22:01:39 +0900)
     */
    public static Symbol schemeSymbol( String string ) {
        if ( string == null )
            return null;
        else
//          return Symbol.valueOf( string );
            return SimpleSymbol.valueOf(string);    
    }
    
    public static String schemeStringToJavaString(Object arg1) {
        return anyToString( arg1 );
    }

    // If it is allowed to return null (in case such return type is Object), then
    // return null. Otherwise, ( in case such return type is double) then throw an
    // error. (Tue, 06 Aug 2019 14:41:05 +0900) 
    public static String anyToString( Object schemeVal ) {
        // modified (Wed, 31 Jul 2019 21:51:55 +0900)
        // reverted (Sun, 04 Aug 2019 21:52:21 +0900)  
        // return toString( value );
        if ( schemeVal == null ) {
            return null;
        } else if ( schemeVal instanceof Symbol ) {
            return fromSymbol(schemeVal);
        } else if ( schemeVal instanceof IString ) {
            return ((IString)schemeVal).toString();
        } else if ( schemeVal instanceof String ) {
            return (String) schemeVal;
        } else if ( schemeVal instanceof Boolean ) {
            return ((Boolean) schemeVal ).toString();
        } else {
            return toString( schemeVal );
        }
    }
    
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
    public static String toString( Object value ) {
        if ( value == null ) 
            return null;
        else
            return value.toString();
    }
    public static String schemeSymbolToJavaString( Object schemeVal ) {
        if ( schemeVal == null ) 
            return null;
        else
            return fromSymbol(schemeVal);
    }
    public static boolean isQuantity( Object schemeVal ) {
        return schemeVal instanceof Quantity;
    }

    public static String fromSymbol(Object schemeVal) {
        return ((Symbol)schemeVal).getName();
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
    
    public static <P,T> List<T> convertList(List<P> list, Function<P,T> f) {
        return list.stream().map(f).collect(Collectors.toList());
    }
    
    public static List<String> symbolListToStringList(List p) {
        return SchemeUtils.<Object,String>convertList((List<Object>)p, (v)->SchemeUtils.schemeSymbolToJavaString(v) );
    }
    public static List<String> schemeStringListToJavaStringList(List p) {
        return SchemeUtils.<Object,String>convertList((List<Object>)p, (v)->SchemeUtils.schemeStringToJavaString(v) );
    }
    public static List<String> anySchemeValueListToStringList(List p) {
        return SchemeUtils.<Object,String>convertList((List<Object>)p, (v)->SchemeUtils.anyToString(v) );
    }

    public static Symbol[] stringListToSymbolList( List<String> stringSymbols ) {
        return stringListToSymbolList( stringSymbols.toArray( new String[ stringSymbols.size()]));
    }

    public static Symbol[] stringListToSymbolList(String ... stringSymbols ) {
        Symbol[] symbols = new Symbol[ stringSymbols.length ];
        for ( int i=0; i<stringSymbols.length; i++ ) {
            symbols[i] = toSchemeSymbol( stringSymbols[i] );
        }
        return symbols;
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
    
    
    public static <T> T errorIfNull( T object ) {
        if ( object == null ) throw new NullPointerException();
        return object;
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
    
    public static Pair acons( String key, Object value ) {
        return Pair.make( toSchemeSymbol( key ) , value );
    }

    private static final boolean DEBUG_ENV = false;


    public static final void defineVar( Environment env, Object value, Collection<String> names ) {
        // env = Environment.getCurrent();
        if ( DEBUG_ENV )
            logInfo( "defineVar:" + env.getName() + ":" + names );
        
        for ( String name : names ) {
            env.define( schemeSymbol( name ), null, value );
        }
    }
    public static final void defineVar( Environment env, Object value, String ... names ) {
        defineVar( env, value, Arrays.asList( names ) );
    }

    public static final void defineVar( Environment env, Procedure value, String ... names ) {
        defineVar( env, value, Arrays.asList( names ) );
    }

    public static final void defineLambda( Environment env, Procedure value ) {
        if ( value instanceof MultipleNamed ) {
            defineVar( env, value, ((MultipleNamed)value).getNames() );
        } else {
            defineVar( env, value, value.getName() );
        }
    }
    public static final boolean isDefined( Environment env, String name  ) {
        // env = Environment.getCurrent();
        if ( DEBUG_ENV )
            logInfo( "isDefined:" + env.getName() + ":" + name );
        synchronized ( Language.getDefaultLanguage() ) {
            return env.isBound( Symbol.valueOf( name ) );
        }
    }
    public static final void putVar( Environment env, String name, Object value ) {
        // env = Environment.getCurrent();
        if ( DEBUG_ENV )
            logInfo( "putVar:" + env.getName() + ":" + name );
        synchronized ( Language.getDefaultLanguage() ) {
            env.put( Symbol.valueOf( name ), null, value );
        }
    }
    public static final Object getVar( Environment env, String name  ) {
        // Environment env = Environment.getCurrent();
        if ( DEBUG_ENV )
            logInfo( "getVar:" + env.getName() + ":" + name );
        synchronized ( Language.getDefaultLanguage() ) {
            return env.get( Symbol.valueOf( name ) );
        }
    }
    // TODO This version is newer. Every getVar() thing should be diverted to this. (Mon, 19 Aug 2019 18:55:39 +0900) 
    public static final <T> T getVar( String name, Object defaultValue ) {
        if ( DEBUG_ENV )
            logInfo( "getVar2:" + Environment.getCurrent().getName() + ":" + name );
        synchronized ( Language.getDefaultLanguage() ) {
            return (T)Environment.getCurrent().get( Symbol.valueOf( name ), defaultValue );
        }
    }
    
    public static void main2(String[] args) {
        Matcher m = Pattern.compile("\\s+").matcher("sasdfasad ffsaddfsa\n\nssadfsd fa");
        System.out.println( m.find() );
        System.out.println( m.start() );
        System.out.println( m.find() );
        System.out.println( m.start() );
        System.out.println( m.find() );
        System.out.println( m.start() );
        
    }
//  public static void main(String[] args) {
//      System.out.println( wrap( "hello world foo bar bum sasdfa s                       sdfsadf sdfasafsadfr", 6 ) );
//  }
    
    
    ////////////////////////////////////////////////////////////////////////////////////////////
    //
    ////////////////////////////////////////////////////////////////////////////////////////////
    
    public static byte[] readAll( InputStream in ) throws IOException {
        ByteArrayOutputStream result = new ByteArrayOutputStream();
        byte[] buf = new byte[1024];
        int length;
        while ((length = in.read(buf)) != -1) {
            result.write(buf, 0, length);
        }
        return result.toByteArray();
    }

    final Procedure reverse = (Procedure)gnu.kawa.slib.srfi1.reverse.get();
    final Procedure map = (Procedure)gnu.kawa.slib.srfi1.map.get();
    //  Object NO_RESULT = EmptyList.emptyList;

//    public LList availableProcedures( Environment environment, int index ) throws Throwable {
//        Procedure1 proc1 = new Procedure1() {
//            @Override
//            public Object apply1(Object arg1) throws Throwable {
//                Pair pair = (Pair)arg1;
//                Object result;
//                if ( index < pair.length() ) {
//                    result = pair.get(index);
//                } else if ( 1 < pair.length() ) {
//                    result =  pair.get(1);
//                } else if ( 0 < pair.length() ) {
//                    result =  Symbol.valueOf(((Procedure)pair.get(0)).getName());
//                } else {
//                    result =  "";
//                }
//                
//                return result;
//            }
//        };
//        return (LList)map.apply2( proc1, 
//                        reverse.apply1( 
//                            DescriptiveDocumentCategory.PROCS.getDocumentList(environment)));
//    }
    public static List<String> getAllKey( Scheme scheme ) {
        ArrayList<String> list = new ArrayList<>();
        Environment env = scheme.getEnvironment();
        for ( LocationEnumeration e=env.enumerateAllLocations();e.hasMoreElements(); ) {
            NamedLocation nl = e.nextElement();
            list.add( schemeSymbolToJavaString( nl.getKeySymbol() ) );
        }
        return list;
    }
    
    public static final String bytesToString( byte[] bs ) {
        return javax.xml.bind.DatatypeConverter.printHexBinary( bs );
    }
    
    
    public static List<String> getAllKey( SchemeEvaluator evaluator ) {
        SchemeResult result = evaluator.evaluate( 
            "(environment-fold (interaction-environment) cons '())", "get-all" );
        result.throwIfError();
        return SchemeUtils.convertList( 
            new ArrayList<>( (LList)result.getValue() ),
            (v)->SchemeUtils.anyToString( v ) );
    }
    
    /////////////////////////////////////////////////////////////////////////////////////////
    // alist
    /////////////////////////////////////////////////////////////////////////////////////////
    private static final Procedure assq = (Procedure) srfi1.assq.get();
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
    public static void main(String[] args) throws Throwable {
        LList alist = LList.makeList( Arrays.asList( 
            Pair.make( Symbol.valueOf( "a" ), Symbol.valueOf( "HELLO" ) ),
            Pair.make( Symbol.valueOf( "b" ), Symbol.valueOf( "WORLD" ) ) ) );
        System.out.println( alistGet( Symbol.valueOf( "a" ), alist, "NOT FOUND" ) );   
    }
    public static String[] symbolsToStrings(Symbol... symbols) {
        String[] strings = new String[ symbols.length ];
        for ( int i=0; i< strings.length; i++ ) {
            strings[i] = schemeSymbolToJavaString( symbols[i] );
        }
        return strings;
    }

}
