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

package ats.pulsar.lib.swing;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import gnu.kawa.io.InPort;
import gnu.kawa.io.Path;
import gnu.lists.AbstractSequence;
import gnu.lists.EmptyList;
import gnu.lists.IString;
import gnu.lists.Pair;
import gnu.mapping.Environment;
import gnu.mapping.SimpleSymbol;
import gnu.mapping.Symbol;
import gnu.math.DFloNum;
import gnu.math.IntNum;
import gnu.math.Quantity;
import kawa.standard.Scheme;

public class SchemeUtils {
	static final Logger LOGGER = Logger.getLogger(SchemeUtils.class.getName());
	static void logError( String msg, Throwable e ) {
		LOGGER.log(Level.SEVERE, msg, e);
	}
	static void logInfo( String msg ) {
//        Logger.getLogger(Pulsar.class.getName()).log(Level.INFO, msg);
		System.err.println( msg );
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
					String key = p.getCar().toString();
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
					System.out.println( e.getClass().getName() );
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
	
	public static Symbol schemeSymbol( String string ) {
		return Symbol.valueOf( string );
	}

	public static String anyToString( Object schemeVal ) {
		if ( schemeVal == null ) {
			return null;
		} else if ( schemeVal instanceof Boolean ) {
			return ((Boolean) schemeVal ).toString();
		} else if ( schemeVal instanceof IString ) {
			return ((IString)schemeVal).toString();
		} else if ( schemeVal instanceof Symbol ) {
			return ((Symbol)schemeVal).getName();
		} else if ( schemeVal instanceof String ) {
			return (String) schemeVal;
		} else {
			return ((Quantity) schemeVal).toString();
		}
	}
	
	public static Boolean toBoolean( Object schemeVal ) {
		if ( schemeVal instanceof Boolean)
			return (Boolean)schemeVal;
		else
			return Boolean.TRUE; // treat everything as #t except #f.
	}
	public static String toString( Object schemeVal ) {
		return schemeVal.toString();
//		if ( schemeVal instanceof String )
//			return (String) schemeVal;
//		else
//			return ((IString)schemeVal).toString();
	}
	public static String symbolToString( Object schemeVal ) {
		return ((Symbol)schemeVal).getName();
	}
	public static double toDouble( Object schemeVal ) {
		return ((Quantity) schemeVal).doubleValue();
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
	
	public static <P,T> Collection<T> convertList(Collection<P> list, Function<P,T> f) {
		return list.stream().map(f).collect(Collectors.toList());
	}
	
	public static Collection<String> symbolListToStringList(Pair p) {
		return SchemeUtils.<Object,String>convertList((Collection<Object>)p.getCdr(), (v)->SchemeUtils.symbolToString(v));
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
	
	@Deprecated
	public static <T> T schemeNullToJavaNull( T object ) {
		if ( object instanceof EmptyList ) {
			return null;
		} else {
			return object;
		}
	}	
	
	/**
	 *  use {@link SchemeUtils#javaNullCheck(Object) }
	 */
	@Deprecated
	public static Object javaNullToSchemeNull( Object object ) {
		if ( object == null )
			return EmptyList.emptyList;
		else
			return object;
	}
	
	public static Object schemeNullCheck( Object object ) {
		if ( (object instanceof Boolean ) && 
             ( ((Boolean)object).booleanValue() == false ) ) 
		{
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
		return new Pair( toSchemeSymbol( key ) , value );
	}

	public static final void defineVar( Scheme scheme, String name, Object value ) {
		scheme.getEnvironment().define( SimpleSymbol.make( "", name ), null, value );
	}
	public static final void putVar( Scheme scheme, String name, Object value ) {
		scheme.getEnvironment().put( SimpleSymbol.make( "", name ), null, value );
	}
	public static final void putVar( Environment e, String name, Object value ) {
		e.put( SimpleSymbol.make( "", name ), null, value );
	}
	public static final boolean isDefined( Scheme scheme, String name  ) {
		return scheme.getEnvironment().isBound( SimpleSymbol.make( "", name ) );
	}
	public static final Object getVar( Scheme scheme, String name  ) {
		return scheme.getEnvironment().get( SimpleSymbol.make( "", name ) );
	}
	
	
	public static void execSchemeFromFile( Scheme scheme, File parentFile, File file) throws FileNotFoundException {
		if ( ! file.isFile() ) {
			throw new FileNotFoundException( file.getPath() );
		}
		
		if ( ! file.isAbsolute() ) {
			if ( parentFile == null )
				throw new FileNotFoundException( "cannot resolve relative path because no parent file is known." );
			
			file = new File( parentFile.getParentFile(), file.getPath() );
		}
		
		InputStream in=null;
		try {
//			String text = new String(Files.readAllBytes( Paths.get( file.toURI() ) ), StandardCharsets.UTF_8);
//			synchronized ( scheme ) {
//				scheme.eval( text );
//			}
			Path path = Path.valueOf(file);
			in = path.openInputStream();
			synchronized ( scheme ) {
				scheme.eval( InPort.openFile( in , path ) );
			}
		} catch (IOException e) {
			logError("", e);
		} catch (Throwable e) {
			logError("", e);
		} finally {
			try {
				if ( in != null )
					in.close();
			} catch (IOException e) {
				logError("", e);
			}
		}
	}
	public static void execScheme( Class parentClass, Scheme scheme, String resourcePath) {
			InputStream in = null;
			try {
				in = parentClass.getResource( resourcePath ).openStream();
				logInfo( "execScheme:" + resourcePath );
	//			scheme.eval( new InputStreamReader(in) );
				scheme.eval( InPort.openFile( in, Path.valueOf( resourcePath ) ) );
			} catch (Throwable e) {
				throw new RuntimeException( e );
			} finally {
				try {
					if ( in != null)
						in.close();
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
		}


}
