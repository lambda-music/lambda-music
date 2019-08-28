package pulsar.lib.scheme;

import java.lang.invoke.MethodHandles;
import java.util.Arrays;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

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
	static Pair getRootCons(Environment e, Symbol symbol) {
		synchronized ( e ) {
			if ( ! e.isBound( symbol ) ) {
				Pair rootCons = (Pair)LList.makeList(Arrays.asList( symbol ));
				e.define( symbol, null, rootCons );
				return rootCons;
			} else {
				return (Pair) e.get( symbol );
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
	public LList getDocumentList( Environment e ) {
		return getDocumentList( e, this );
	}
	private static LList getDocumentList( Environment e, DescriptiveDocumentType type ) {
		synchronized ( e ) {
			Pair rootCons = getRootCons( e, type.getSymbol() );
			return (LList) rootCons.getCdr();
		}
	}

	public void addDocumentList( Environment e, Symbol[] symbols, Object descriptive ) {
		addDocumentList( e, this, symbols, descriptive );
	}
	private static void addDocumentList( Environment e, DescriptiveDocumentType type, Symbol[] symbols, Object descriptive ) {
		synchronized ( e ) {
			Pair rootCons = getRootCons( e, type.getSymbol() );
			rootCons.setCdr( 
				Pair.make(
					Pair.make( descriptive, LList.makeList( symbols, 0 ) ),  
					rootCons.getCdr()));
			
			//			proc.setNameList( Arrays.asList(symbols) );
		}
	}
	public Object defineDoc( Scheme scheme, DescriptiveBean bean ) {
		return defineDoc( scheme, bean, this );
	}
	public static Object defineProcDoc( Scheme scheme, DescriptiveBean bean ) {
		return defineDoc( scheme, bean, DescriptiveDocumentType.PROCS );
	}
	public static Object defineNoteDoc( Scheme scheme, DescriptiveBean bean ) {
		return defineDoc( scheme, bean, DescriptiveDocumentType.NOTES );
	}
	static Object defineDoc(Scheme scheme, DescriptiveBean bean, DescriptiveDocumentType documentType) {
		Object proc = defineDoc0( scheme, documentType, bean.format(), bean.getName(), bean.getNames() );
		SchemeUtils.setDescriptionBean( proc, bean );
		return proc;
	}
	
	//		static Procedure proc_defineDocument = eval( lis( "lambda", lis("rt"),   ) );   
	public static Object defineDoc0( Scheme scheme, DescriptiveDocumentType documentType, String description, String name, List<String> names )  {
		logInfo( "DescriptiveDocumentType.defineDoc0()" + name );
		synchronized ( scheme ) {
			Object proc = SchemeUtils.getVar( scheme, name, null );
			if ( proc == null ) {
				SchemeUtils.logWarn( "setDocumentInitializer: " + name + " was not found." );
				proc = new DescriptiveHelpProcedure( name );
				SchemeUtils.defineVar( scheme, proc, name );
			}
			logInfo( "description" );
			logInfo( description );
			SchemeUtils.setDescription( proc, description );
			addDocumentList( 
				scheme.getEnvironment(),
				documentType,
				SchemeUtils.stringListToSymbolList( names ), 
				proc );
			
			return proc;
		}
	}
	
}