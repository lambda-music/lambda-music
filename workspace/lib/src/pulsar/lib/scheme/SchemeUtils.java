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

import java.io.ByteArrayOutputStream;
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
import java.lang.invoke.MethodHandles;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
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
import gnu.kawa.io.InPort;
import gnu.kawa.io.OutPort;
import gnu.kawa.io.Path;
import gnu.lists.AbstractSequence;
import gnu.lists.Consumer;
import gnu.lists.EmptyList;
import gnu.lists.FString;
import gnu.lists.IString;
import gnu.lists.LList;
import gnu.lists.Pair;
import gnu.mapping.CallContext;
import gnu.mapping.Environment;
import gnu.mapping.Procedure;
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
	static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
	static void logError(String msg, Throwable e) {
		LOGGER.log(Level.SEVERE, msg, e);
	}
	static void logInfo(String msg) {
		LOGGER.log(Level.INFO, msg);
	}
	static void logWarn(String msg) {
		LOGGER.log(Level.WARNING, msg);
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
//		System.out.println(env);
//		System.out.println(llist);
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
//	public static void main(String[] args) throws Throwable {
//		testEval2();
//	}
	public static Pair symbols(String ... args) {
		return (Pair)Pair.makeList( javaStringListToSchemeSymbolList(Arrays.asList(args)) );
	}

	public static List<Symbol> javaStringListToSchemeSymbolList( List<String> stringList) { 
		return SchemeUtils.<String,Symbol>convertList( stringList, (o)->{
			return SchemeUtils.schemeSymbol( o );
		});
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
	
	public static List<String> symbolListToStringList(List p) {
		return SchemeUtils.<Object,String>convertList((List<Object>)p, (v)->SchemeUtils.symbolToString(v) );
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
		
		if ( value instanceof Descriptive) {
			addDocumentList(e, symbols, (Descriptive)value);
		}
	}

//	public static void setDocumentInitializer( Scheme scheme, DescriptiveInitializerBean bean, DescriptiveInitializerBeanParam beanParam, String ... names ) {
//		if ( names.length < 1 )
//			throw new IllegalArgumentException( "the 'names' parameter must be longer than 1." );
//		setDocument( scheme, names[0], bean.process( beanParam ).format() );
//	}

	public static final String ALL_PROCEDURES = "all-procedures";
	public static final Symbol ALL_PROCEDURES_SYMBOL = Symbol.valueOf( ALL_PROCEDURES ); 
	public static Pair getRootOfAllProcedure(Environment e) {
		synchronized ( e ) {
			if ( ! e.isBound( ALL_PROCEDURES_SYMBOL ) ) {
				Pair root = (Pair)LList.makeList(Arrays.asList( ALL_PROCEDURES_SYMBOL ));
				e.define( ALL_PROCEDURES_SYMBOL, null, root);
				return root;
			} else {
				return (Pair) e.get( ALL_PROCEDURES_SYMBOL );
			}
		}
	}
	public static LList getDocumentList( Environment e ) {
		synchronized ( getRootOfAllProcedure(e) ) {
			return (LList) getRootOfAllProcedure(e).getCdr();
		}
	}
	public static void addDocumentList( Environment e, Symbol[] symbols, Descriptive descriptive ) {
		synchronized ( e ) {
			Pair root = getRootOfAllProcedure(e);
			root.setCdr( 
				Pair.make(
					Pair.make( descriptive, LList.makeList( symbols, 0 ) ),  
					root.getCdr()));
			
//			proc.setNameList( Arrays.asList(symbols) );
		}
	}

//	public static void defineDocument( Environment e, String[] stringSymbols, DescriptiveProcedure proc ) {
//		Symbol[] symbols = new Symbol[ stringSymbols.length ];
//		for ( int i=0; i<stringSymbols.length; i++ ) {
//			symbols[i] = toSchemeSymbol( stringSymbols[i] );
//		}
//		defineDocument( e, symbols, proc );
//	}
//
//	public static DescriptiveProcedure getDocument( Environment e, Symbol name )  {
//	synchronized ( e ) {
//		Pair root = getRootOfAllProcedure(e);
//		for ( Object o : root ) {
//			if (((Pair)
//					((Pair)o).getCdr()).contains( name )) {
//				return (DescriptiveProcedure)((Pair)o).getCar();
//			}
//		}
//		return null;
//	}
//}
	public static void defineDoc( Scheme scheme, DescriptiveInitializerBean bean, String ... names ) {
		if ( names.length < 1 )
			throw new IllegalArgumentException( "the 'names' parameter must be longer than 1." );
		bean.setNameList( names ); 
		defineDoc0( scheme, names[0], bean.format() );
	}

//	static Procedure proc_defineDocument = eval( lis( "lambda", lis("rt"),   ) );   
	static void defineDoc0( Scheme scheme, String name, String description )  {
		synchronized ( scheme ) {
			DescriptiveProcedure proc = getVar( scheme, name, null );
			if ( proc == null ) {
				logWarn( "setDocumentInitializer: " + name + " was not found." );
				proc = new DHelp( name );
				defineVar( scheme, proc, name );
			}
			proc.setDescription( description );
		}
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
	// TODO This version is newer. Every getVar() thing should be diverted to this. (Mon, 19 Aug 2019 18:55:39 +0900) 
	public static final <T> T getVar( Scheme scheme, String name, Object defaultValue ) {
		return (T)scheme.getEnvironment().get( Symbol.valueOf( name ), defaultValue );
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
	
	public static Object prettyPrintReconstruction( Object o ) {
		return prettyPrintReconstruction0( new ArrayDeque<>(), o );
	}
	private static Object prettyPrintReconstruction0(ArrayDeque stack, Object o) {
		if ( stack.contains(o) ) {
			return o;
		}
		try {
			stack.push( o );
			if ( o instanceof EmptyList ) {
				return o;
			} else if ( o instanceof Pair ) {
				Pair p = (Pair)o;
				return Pair.make(
					prettyPrintReconstruction0( stack, p.getCar()),
					prettyPrintReconstruction0( stack, p.getCdr()));
			} else if ( o instanceof Symbol ) {
				return toSchemeString( "'" + symbolToString( (Symbol)o ) );
			} else if ( o instanceof IString || o instanceof FString ) {
				return toSchemeString( "\"" + anyToString( o ) + "\"" );
			} else {
				return o;
			}
		} finally {
			stack.pop();
		}
	}
	
	public static String normalPrint(Object resultObject) throws Throwable {
		return printProc( kawa.lib.ports.display, resultObject );
	}

	/*
	 * moved from KawaPad.java (Mon, 29 Jul 2019 19:31:42 +0900)
	 */
	// TODO integrate all prettyPrint things.
	public static String prettyPrint(Object resultObject) throws Throwable {
		return printProc(
			kawa.lib.kawa.pprint.pprint,
			prettyPrintReconstruction( resultObject ));
	}
	private static String printProc(Procedure print, Object resultObject) throws Throwable {
		StringWriter out = new StringWriter();
		try {
			OutPort outPort = new OutPort( out, true, true );
			SchemeUtils.toString( print.apply2( resultObject, outPort ) );
//			SchemeUtils.toString( kawa.lib.ports.write.apply2( resultObject, outPort ) );
			outPort.flush();
			return out.toString();
		} finally {
			out.close();
		}
	}
	public static String wrapMultiLine( String s, int width ) {
		StringBuilder sb = new StringBuilder();
//		String[] a = s.split("\\r\\n\\r\\n|\\n\\r\\n\\r|\\n\\n|\\r\\r" );
		String[] a = s.split( "\n\n" );
		for ( int i=0; i<a.length; i++ ) {
			 sb.append( wrap(a[i],width).trim() ).append( "\n\n" );
		}
		return sb.toString();
	}
	
	public static String prefixMultiLine( String s, String prefix ) {
		StringBuilder sb = new StringBuilder();
		String[] a = s.split( "\n" );
		for ( int i=0; i<a.length; i++ ) {
			 sb.append(prefix).append( a[i] ).append( "\n" );
		}
		return sb.toString();
	}
	
	public static String wrap( String s, int width ) {
		Matcher m = Pattern.compile( "\\s+" ).matcher(s);
		StringBuilder sb = new StringBuilder();
		int head=0;
		int last=head;
		while ( m.find() ) {
			int curr = m.start();
			String stringToAdd = s.substring(last,curr);
			if ( ( curr-head ) < width ) {
				sb.append(' ');
				sb.append( stringToAdd );
				last = curr + m.group().length();
			} else {
				sb.append('\n');
				sb.append( stringToAdd );
				last = curr + m.group().length();
				head = last;
			}
		}
		{
			String stringToAdd = s.substring(last,s.length());
			sb.append(' ');
			sb.append( stringToAdd );
			sb.append('\n');
		}
		return sb.toString();
	}
	public static void main(String[] args) {
		Matcher m = Pattern.compile("\\s+").matcher("sasdfasad ffsaddfsa\n\nssadfsd fa");
		System.out.println( m.find() );
		System.out.println( m.start() );
		System.out.println( m.find() );
		System.out.println( m.start() );
		System.out.println( m.find() );
		System.out.println( m.start() );
		
	}
//	public static void main(String[] args) {
//		System.out.println( wrap( "hello world foo bar bum sasdfa s                       sdfsadf sdfasafsadfr", 6 ) );
//	}
	
	
	
	public static final class ExecuteSchemeResult {
		public final boolean isDocument;
		public final String result;
		public final Throwable error;
		public final boolean succeeded() {
			return error == null;
		}
		public ExecuteSchemeResult(boolean isDocument, String result, Throwable error) {
			super();
			this.isDocument = isDocument;
			this.result = result;
			this.error = error;
		}
	}
	public static final String EXECUTE_SCHEME_DOCTAG_STRING = "**doc**".intern();
	public static final Symbol EXECUTE_SCHEME_DOCTAG = Symbol.valueOf( EXECUTE_SCHEME_DOCTAG_STRING );
	public static Object executeSchemePageWrapper( Object o ) {
		return Pair.make( EXECUTE_SCHEME_DOCTAG, o );
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
			// But I think it is cumbersome to ask users to use load-relative procedure in
			// every situation. IMO load-relative supposed to be default.
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
					return new ExecuteSchemeResult( false, "#!null", null );
				} else {
					if ( result instanceof Pair &&
							EXECUTE_SCHEME_DOCTAG.equals( ((Pair) result).getCar() ) ) 
					{
						return new ExecuteSchemeResult( true, normalPrint(((Pair) result).getCdr()), null );
					} else {
						return new ExecuteSchemeResult( false, prettyPrint(result), null );
					}
				}
			} catch (Throwable e) {
				StringWriter sw = new StringWriter();
				PrintWriter w = new PrintWriter( sw );
				try {
					e.printStackTrace( w );
					w.flush();
					sw.flush();
					return new ExecuteSchemeResult( false, sw.toString(), e );
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
	
	public static byte[] readAll( InputStream in ) throws IOException {
		ByteArrayOutputStream result = new ByteArrayOutputStream();
		byte[] buf = new byte[1024];
		int length;
		while ((length = in.read(buf)) != -1) {
		    result.write(buf, 0, length);
		}
		return result.toByteArray();
	}
	public static Object makePage( Object o ) {
		return executeSchemePageWrapper( o );
	}
	public static Object makePage( String message, int textWidth ) {
		message = SchemeUtils.wrapMultiLine( message, textWidth ).trim() + "\n";
		message = 
				"#|\n"+
				SchemeUtils.prefixMultiLine( message, "   " )+
				"  |# help about-intro";
		return SchemeUtils.makePage( SchemeUtils.toSchemeString( message ) );
	};

	
}
