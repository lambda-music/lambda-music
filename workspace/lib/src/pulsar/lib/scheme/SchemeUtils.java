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

package pulsar.lib.scheme;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import gnu.expr.Language;
import gnu.kawa.io.InPort;
import gnu.kawa.io.OutPort;
import gnu.kawa.io.Path;
import gnu.lists.AbstractSequence;
import gnu.lists.Consumer;
import gnu.lists.EmptyList;
import gnu.lists.IString;
import gnu.lists.LList;
import gnu.lists.Pair;
import gnu.mapping.CallContext;
import gnu.mapping.Environment;
import gnu.mapping.SimpleSymbol;
import gnu.mapping.Symbol;
import gnu.math.DFloNum;
import gnu.math.IntNum;
import gnu.math.Quantity;
import kawa.Shell;
import kawa.standard.Scheme;
import pulsar.lib.scheme.scretary.SchemeSecretary;
import pulsar.lib.secretary.Invokable;
import pulsar.lib.secretary.SecretaryMessage;

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
					String key   = fromSymbol( p.getCar() );
//					String key   = p.getCar().toString();   //XXX TODO
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
	
	/*
	 * We changed the policy the way to treat null here;
	 * now we are ignoring null value. (Sun, 04 Aug 2019 22:01:39 +0900)
	 */
	public static Symbol schemeSymbol( String string ) {
//		return Symbol.valueOf( string );
		return SimpleSymbol.valueOf(string);	
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
	public static String symbolToString( Object schemeVal ) {
		if ( schemeVal == null ) 
			return null;
		else
			return fromSymbol(schemeVal);
	}
	public static String fromSymbol(Object schemeVal) {
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
	
	public static <P,T> List<T> convertList(List<P> list, Function<P,T> f) {
		return list.stream().map(f).collect(Collectors.toList());
	}
	
	public static List<String> symbolListToStringList(Pair p) {
		return SchemeUtils.<Object,String>convertList((List<Object>)p, (v)->SchemeUtils.symbolToString(v) );
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

	public static final Symbol SYMBOL_ALL_PROCEDURES = Symbol.valueOf( "all-procedures" ); 
	
	public static final void defineVar( Scheme scheme, Object value, String ... names ) {
		Environment e = scheme.getEnvironment();
		Environment.setCurrent(e);
		Language.setCurrentLanguage(scheme);

		Symbol[] symbols = new Symbol[names.length];
		for ( int i=0; i<names.length; i++ ) {
			if ( names[i] != null ) {
				Symbol symbol = schemeSymbol( names[i] );
				symbols[i] = symbol;
				e.define( symbol, null, value );
			}
		}
		
		if ( value instanceof DescriptiveProcedure ) {
			defineDocument(e, symbols, (DescriptiveProcedure)value);
		}
	}
	static void defineDocument( Environment e, Symbol[] symbols, DescriptiveProcedure proc) {
		if ( ! e.isBound( SYMBOL_ALL_PROCEDURES  ) ) {
			e.define(SYMBOL_ALL_PROCEDURES, null, LList.makeList( Collections.EMPTY_LIST ) );
		}

		Pair pair = Pair.make(
			Pair.make( proc, LList.makeList( symbols, 0 )),  
			(LList) e.get(SYMBOL_ALL_PROCEDURES));
		e.put( SYMBOL_ALL_PROCEDURES, pair );
	}
	public static final boolean isDefined( Scheme scheme, String name  ) {
		return scheme.getEnvironment().isBound( SimpleSymbol.make( "", name ) );
	}
	public static final void putVar( Scheme scheme, String name, Object value ) {
		scheme.getEnvironment().put( SimpleSymbol.make( "", name ), null, value );
	}
	public static final Object getVar( Scheme scheme, String name  ) {
		return scheme.getEnvironment().get( SimpleSymbol.make( "", name ) );
	}
	
	
	@Deprecated
	public static void execSchemeFromFileOld( Object lock, Scheme scheme, File file) throws FileNotFoundException {
		if ( ! file.isFile() ) {
			throw new FileNotFoundException( file.getPath() );
		}
			
		InputStream in=null;
		try {
//			String text = new String(Files.readAllBytes( Paths.get( file.toURI() ) ), StandardCharsets.UTF_8);
//			synchronized ( scheme ) {
//				scheme.eval( text );
//			}
			Path path = Path.valueOf(file);
			in = path.openInputStream();
			synchronized ( lock ) {
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
	public static void execSchemeFromFile( Scheme scheme, File file) throws FileNotFoundException {
		execScheme( scheme, new FileInputStream(file), file.getName() );
	}
	public static void execScheme( Class parentClass, Scheme scheme, String resourcePath ) {
		try {
			execScheme( scheme, parentClass.getResource( resourcePath ).openStream(), resourcePath );
		} catch (IOException e) {
			throw new RuntimeException(e); 
		}
	}
	public static void execScheme( Scheme scheme, InputStream in, String resourcePathAlias ) {
		evaluateScheme( scheme, null, new InputStreamReader( in ), resourcePathAlias );
	}

	public static void execScheme1( Scheme scheme, InputStream in, String resourcePathAlias ) {
		synchronized ( scheme ) {
			try {
				logInfo( "execScheme:" + resourcePathAlias );
				scheme.eval( InPort.openFile( in, Path.valueOf( resourcePathAlias ) ) );
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
	@Deprecated
	public static Object kawaExecuteScheme( Map<String,Object> variables, Scheme scheme, String script, String scriptNameURI ) throws Throwable {
		synchronized ( scheme ) {
			StringReader reader = new StringReader(script);
			try {
				if ( variables != null )
					for ( Map.Entry<String, Object> e : variables.entrySet() )
						putVar( scheme , e.getKey(), e.getValue() );
//				putVar( scheme , "scheme", scheme  );
//				putVar( scheme , "frame", kawaPad );
				return scheme .eval( new InPort( reader, Path.valueOf( scriptNameURI ))); 
			} finally {
				reader.close();
				if ( variables != null )
					for ( Map.Entry<String, Object> e : variables.entrySet() )
						putVar( scheme , e.getKey(), false );
//				putVar( scheme , "scheme", false );
//				putVar( scheme , "frame", false );
			}
		}
	}
	
	// TODO integrate all prettyPrint things.
	public static String prettyPrint( Object resultObject ) throws Throwable {
		return SimpleSchemePrettifier.prettyPrint(resultObject);
	}

	public static final class ExecuteSchemeResult {
		public final String result;
		public final Throwable error;
		public final boolean succeeded() {
			return error == null;
		}
		public ExecuteSchemeResult(String result, Throwable error) {
			super();
			this.result = result;
			this.error = error;
		}
	}

	public static ExecuteSchemeResult evaluateScheme( 
			Scheme scheme, Map<String, Object> variables, Reader schemeScript, String schemeScriptURI) 
	{
		//				schemeSecretary.initializeSchemeForCurrentThread();
		synchronized ( scheme ) {
			SchemeSecretary.initializeSchemeForCurrentThreadStatic( scheme );
			
			if ( variables != null )
				for ( Map.Entry<String, Object> e : variables.entrySet() )
					putVar( scheme , e.getKey(), e.getValue() );
			
			putVar( scheme , "scheme", scheme );
			
			
			// Set current directory to the default load path.
			// Note that <i>Shell.currentLoadPath</i> is not documented in the official documentation.
			// The variable Shell.currentLoadPath is only referred in kawa.standard.load and 
			// this is the only way to affect the //load//'s behavior.
			
			// FIXME
			// Now I realized that currentLoadPath only affect to (load-relative) and
			// it will by no means affect to (load) sigh. 
			// This code effectively makes (load-relative) current directory aware.
			// But I think it is cumbersome for users to use load-relative procedure.
			// IMO load-relative supposed to be default. 
			// I'm thinking about it.  (Thu, 15 Aug 2019 16:21:22 +0900)
			Path savedPath = (Path) Shell.currentLoadPath.get();
			try {
				Shell.currentLoadPath.set( Path.valueOf( new File(".").getAbsoluteFile().getCanonicalFile() ) );

				CallContext ctx = CallContext.getInstance();
				Consumer out = Shell.getOutputConsumer(OutPort.outDefault());
				if (out != null)
				{
					ctx.consumer = out;
				}
				
				
				 // {@link kawa.Shell#runFile(InputStream, Path, gnu.mapping.Environment, boolean, int) }
				Object result = scheme.eval( new InPort( schemeScript, Path.valueOf( schemeScriptURI ) ) );
				// Object result = Shell.run( schemeScript, schemeScriptURI, scheme.getEnvironment(), true, 0 ); 

				if ( result == null ) {
					return new ExecuteSchemeResult( "#!null", null );
				} else {
					return new ExecuteSchemeResult( prettyPrint(result), null );
				}
			} catch (Throwable e) {
				StringWriter sw = new StringWriter();
				PrintWriter w = new PrintWriter( sw );
				try {
					e.printStackTrace( w );
					w.flush();
					sw.flush();
					return new ExecuteSchemeResult( sw.toString(), e );
				} finally {
					try {
						sw.close();
					} catch (IOException e1) {
						e1.printStackTrace();
					}
					w.close();
				}
			} finally {
				putVar( scheme , "scheme", false );
				
				if ( variables != null )
					for ( Map.Entry<String, Object> e : variables.entrySet() )
						putVar( scheme , e.getKey(), false );
				
				try {
					schemeScript.close();
				} catch (IOException e1) {
					logError( "failed to close the stream" , e1 );
				}
				
				Shell.currentLoadPath.set( savedPath );
			}
		}
	}
		
	public static ExecuteSchemeResult evaluateScheme( SchemeSecretary schemeSecretary, Map<String,Object> variables, String schemeScript, String schemeScriptURI ) {
		return evaluateScheme( schemeSecretary, variables, new StringReader( schemeScript ),  schemeScriptURI );
	}
	public static ExecuteSchemeResult evaluateScheme( SchemeSecretary schemeSecretary, Map<String,Object> variables, Reader schemeScript, String schemeScriptURI ) {
		return schemeSecretary.executeSecretarially( new SecretaryMessage.NoThrow<Scheme,ExecuteSchemeResult>() {
			@Override
			public ExecuteSchemeResult execute0(Scheme scheme, Object[] args) {
				return evaluateScheme(scheme, variables, schemeScript, schemeScriptURI);
			}
		}, Invokable.NOARG );
	}

	
	public static String endWithLineFeed(String s) {
		if ( s == null )
			return null;
		else if ( s.equals( "" ) )
			return "";
		else if ( s.endsWith("\n" ) )
			return s;
		else
			return s + "\n"; 
	}
	public static String formatResult( String s ) {
		if ( s == null )
			return null;
		else if ( s.equals( "" ) )
			return "";
		else
			return "#|\n" + endWithLineFeed( s ) + "|#\n";
	}
}
