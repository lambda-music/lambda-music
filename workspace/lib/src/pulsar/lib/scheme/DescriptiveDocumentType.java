package pulsar.lib.scheme;

import java.lang.invoke.MethodHandles;
import java.util.Arrays;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import gnu.expr.Language;
import gnu.lists.LList;
import gnu.lists.Pair;
import gnu.mapping.Environment;
import gnu.mapping.Symbol;
import kawa.standard.Scheme;


public enum DescriptiveDocumentType {
	PROCS( Symbol.valueOf( "all-procedures" )),
	NOTES( Symbol.valueOf( "all-notation-types" )),
	;
	static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
	static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE,   msg, e   ); }
	static void logInfo (String msg             ) { LOGGER.log(Level.INFO,     msg      ); }
	static void logWarn (String msg             ) { LOGGER.log(Level.WARNING,  msg      ); }

//	public static void defineDocument( Environment e, String[] stringSymbols, DescriptiveProcedure proc ) {
//		Symbol[] symbols = new Symbol[ stringSymbols.length ];
//		for ( int i=0; i<stringSymbols.length; i++ ) {
//			symbols[i] = toSchemeSymbol( stringSymbols[i] );
//		}
//		defineDocument( e, symbols, proc );
//	}
//	
//	public static DescriptiveProcedure getDocument( Environment e, Symbol name )  {
//		synchronized ( e ) {
//			Pair root = getRootOfAllProcedure(e);
//			for ( Object o : root ) {
//				if (((Pair)
//						((Pair)o).getCdr()).contains( name )) {
//					return (DescriptiveProcedure)((Pair)o).getCar();
//				}
//			}
//			return null;
//		}
//	}
//	public static void setDocumentInitializer( Scheme scheme, DescriptiveInitializerBean bean, DescriptiveInitializerBeanParam beanParam, String ... names ) {
//		if ( names.length < 1 )
//			throw new IllegalArgumentException( "the 'names' parameter must be longer than 1." );
//		setDocument( scheme, names[0], bean.process( beanParam ).format() );
//	}
	public static DescriptiveDocumentType lookFor( String s ) {
		if ( s == null ) 
			return null;
		try {
			return valueOf( s );
		} catch ( IllegalArgumentException e ) {
			return null;
		}
		
	}
	public Symbol toSymbol() {
		String str = this.toString();
		str = str.replaceAll( "_" , "-" );
		str = str.toLowerCase();
		return Symbol.valueOf( str );
	}
	public static DescriptiveDocumentType valueOf( Symbol symbol ) {
		String str = SchemeUtils.symbolToString( symbol );
		str = str.replaceAll( "-", "_" );
		str = str.toUpperCase();
		return DescriptiveDocumentType.valueOf( str ); 
	}
	static Pair getRootCons( Environment env, Symbol symbol) {
		synchronized ( Language.getDefaultLanguage() ) {
			if ( ! env.isBound( symbol ) ) {
				Pair rootCons = (Pair)LList.makeList(Arrays.asList( symbol ));
				env.define( symbol, null, rootCons );
				return rootCons;
			} else {
				return (Pair)env.get( symbol );
			}
		}
	}
	private Symbol symbol;
	private DescriptiveDocumentType( Symbol symbol ) {
		this.symbol = symbol;
	}
	public Symbol getSymbol() {
		return symbol;
	}
	public LList getDocumentList( Environment env ) {
		return getDocumentList( env, this );
	}
	private static LList getDocumentList( Environment env, DescriptiveDocumentType type ) {
		synchronized ( env ) {
			Pair rootCons = getRootCons( env, type.getSymbol() );
			return (LList) rootCons.getCdr();
		}
	}

	public void addDocumentList( Environment env, Symbol[] symbols, Object descriptive ) {
		addDocumentList( env, this, symbols, descriptive );
	}
	private static void addDocumentList( Environment env, DescriptiveDocumentType type, Symbol[] symbols, Object descriptive ) {
		synchronized ( Language.getDefaultLanguage() ) {
			Pair rootCons = getRootCons( env, type.getSymbol() );
			rootCons.setCdr( 
				Pair.make(
					Pair.make( descriptive, LList.makeList( symbols, 0 ) ),  
					rootCons.getCdr()));
			
			//			proc.setNameList( Arrays.asList(symbols) );
		}
	}

	public Object defineDoc( Scheme scheme, DescriptiveBean bean ) {
		return defineDoc( scheme.getEnvironment(), bean, this );
	}
	public static Object defineProcDoc( Scheme scheme, DescriptiveBean bean ) {
		return defineDoc( scheme.getEnvironment(), bean, DescriptiveDocumentType.PROCS );
	}
	public static Object defineNoteDoc( Scheme scheme, DescriptiveBean bean ) {
		return defineDoc( scheme.getEnvironment(), bean, DescriptiveDocumentType.NOTES );
	}
	static Object defineDoc(Environment env, DescriptiveBean bean, DescriptiveDocumentType documentType) {
		return documentType.defineDoc( env, bean );
	}
	public Object defineDoc( Environment env, DescriptiveBean bean ) {
		synchronized ( Language.getDefaultLanguage() ) {
			Object proc = defineDoc0( env, this, bean.format(), bean.getName(), bean.getNames() );
			SchemeUtils.setDescriptionBean( proc, bean );
			return proc;
		}
	}

	//		static Procedure proc_defineDocument = eval( lis( "lambda", lis("rt"),   ) );   
	public static Object defineDoc0( Environment env, DescriptiveDocumentType documentType, String description, String name, List<String> names )  {
		logInfo( "DescriptiveDocumentType.defineDoc0()" + name );
		Object proc = SchemeUtils.getVar( name, null );
		if ( proc == null ) {
			SchemeUtils.logWarn( "setDocumentInitializer: " + name + " was not found." );
			proc = new DescriptiveHelpProcedure( name );
			SchemeUtils.defineVar( proc, name );
		}
		logInfo( "setting description on '" + name + "'" + " " + proc.toString() );
		//			logInfo( "description" );
		//			logInfo( description );
		SchemeUtils.setDescription( proc, description );
		addDocumentList(
			env,
			documentType,
			SchemeUtils.stringListToSymbolList( names ), 
			proc );
		
		return proc;
	}
	
}