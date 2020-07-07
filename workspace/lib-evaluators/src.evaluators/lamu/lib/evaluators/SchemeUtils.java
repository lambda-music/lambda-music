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

package lamu.lib.evaluators;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.logging.Level;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import gnu.expr.Language;
import gnu.lists.IString;
import gnu.lists.LList;
import gnu.lists.Pair;
import gnu.mapping.Environment;
import gnu.mapping.LocationEnumeration;
import gnu.mapping.NamedLocation;
import gnu.mapping.Procedure;
import gnu.mapping.SimpleSymbol;
import gnu.mapping.Symbol;
import gnu.math.Quantity;
import kawa.standard.Scheme;
import lamu.lib.kawautils.SchemeValues;
import lamu.lib.logging.Logger;

public class SchemeUtils {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg) { LOGGER.log(Level.INFO, msg); }
    static void logWarn(String msg) { LOGGER.log(Level.WARNING, msg); }
   
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
                result = new Pair( SchemeValues.toSchemeSymbol((String)vals[i]), result );
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
        return SchemeValues.anyToString( arg1 );
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

    public static List<String> toStringList(List<?> list) {
        return list.stream().map( (v)->v==null ? "null" : v.toString() ).collect(Collectors.toList());
    }
    

    public static Symbol[] stringListToSymbolList( List<String> stringSymbols ) {
        return stringListToSymbolList( stringSymbols.toArray( new String[ stringSymbols.size()]));
    }

    public static Symbol[] stringListToSymbolList(String ... stringSymbols ) {
        Symbol[] symbols = new Symbol[ stringSymbols.length ];
        for ( int i=0; i<stringSymbols.length; i++ ) {
            symbols[i] = SchemeValues.toSchemeSymbol( stringSymbols[i] );
        }
        return symbols;
    }
                                                                    
    
    public static Pair acons( String key, Object value ) {
        return Pair.make( SchemeValues.toSchemeSymbol( key ) , value );
    }

    static final boolean DEBUG_ENV = false;


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
            list.add( SchemeValues.toString(nl.getKeySymbol()) );
        }
        return list;
    }
    
    public static final String bytesToString( byte[] bs ) {
        return javax.xml.bind.DatatypeConverter.printHexBinary( bs );
    }
    
    
    public static String[] symbolsToStrings(Symbol... symbols) {
        String[] strings = new String[ symbols.length ];
        for ( int i=0; i< strings.length; i++ ) {
            strings[i] = SchemeValues.toString(symbols[i]);
        }
        return strings;
    }
    
    
}
