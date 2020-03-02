package quartz.lib.scheme.doc;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.lang.invoke.MethodHandles;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;

import gnu.expr.Language;
import gnu.lists.LList;
import gnu.lists.Pair;
import gnu.mapping.Environment;
import gnu.mapping.Symbol;
import quartz.lib.log.PulsarLogger;
import quartz.lib.scheme.SchemeUtils;


public class DescriptiveDocumentCategory {
    public static DescriptiveDocumentCategory createCategory(Symbol symbol) {
        return new DescriptiveDocumentCategory( symbol );
    }
    public static DescriptiveDocumentCategory createCategory(String symbol) {
        return new DescriptiveDocumentCategory( SchemeUtils.schemeSymbol( symbol ) );
    }

    static final Map<Symbol,DescriptiveDocumentCategory> allCategories = new LinkedHashMap<>();
    public static synchronized void addCategory(DescriptiveDocumentCategory category) {
        allCategories.put( category.symbol, category );
    }
    public static synchronized void removeCategory(DescriptiveDocumentCategory category) {
        allCategories.remove( category.symbol );
    }
    public static synchronized Collection<DescriptiveDocumentCategory> getAll() {
        return new ArrayList<>( allCategories.values() );
    }

    private static final boolean DEBUG = false; 
    static final PulsarLogger LOGGER = PulsarLogger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE,   msg, e   ); }
    static void logInfo (String msg             ) { LOGGER.log(Level.INFO,     msg      ); }
    static void logWarn (String msg             ) { LOGGER.log(Level.WARNING,  msg      ); }

//  public static void defineDocument( Environment e, String[] stringSymbols, DescriptiveProcedure proc ) {
//      Symbol[] symbols = new Symbol[ stringSymbols.length ];
//      for ( int i=0; i<stringSymbols.length; i++ ) {
//          symbols[i] = toSchemeSymbol( stringSymbols[i] );
//      }
//      defineDocument( e, symbols, proc );
//  }
//  
//  public static DescriptiveProcedure getDocument( Environment e, Symbol name )  {
//      synchronized ( e ) {
//          Pair root = getRootOfAllProcedure(e);
//          for ( Object o : root ) {
//              if (((Pair)
//                      ((Pair)o).getCdr()).contains( name )) {
//                  return (DescriptiveProcedure)((Pair)o).getCar();
//              }
//          }
//          return null;
//      }
//  }
//  public static void setDocumentInitializer( Scheme scheme, DescriptiveInitializerBean bean, DescriptiveInitializerBeanParam beanParam, String ... names ) {
//      if ( names.length < 1 )
//          throw new IllegalArgumentException( "the 'names' parameter must be longer than 1." );
//      setDocument( scheme, names[0], bean.process( beanParam ).format() );
//  }
    public synchronized static DescriptiveDocumentCategory valueOf(Symbol symbol) {
        return allCategories.get( symbol );
    }
    public synchronized static DescriptiveDocumentCategory valueOf(String symbolString ) {
        return allCategories.get( Symbol.valueOf( symbolString ) );
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
    private DescriptiveDocumentCategory( Symbol symbol ) {
        this.symbol = symbol;
        DescriptiveDocumentCategory.addCategory( this );
    }
    public Symbol getSymbol() {
        return symbol;
    }
    public LList getDocumentList( Environment env ) {
        return getDocumentList( env, this );
    }
    private static LList getDocumentList( Environment env, DescriptiveDocumentCategory type ) {
        synchronized ( Language.getDefaultLanguage() ) {
            Pair rootCons = getRootCons( env, type.getSymbol() );
            return (LList) rootCons.getCdr();
        }
    }

    public void addDocumentList( Environment env, Symbol[] symbols, Object descriptive ) {
        addDocumentList( env, this, symbols, descriptive );
    }
    private static void addDocumentList( Environment env, DescriptiveDocumentCategory type, Symbol[] symbols, Object descriptive ) {
        synchronized ( Language.getDefaultLanguage() ) {
            Pair rootCons = getRootCons( env, type.getSymbol() );
            rootCons.setCdr( 
                Pair.make(
                    Pair.make( descriptive, LList.makeList( symbols, 0 ) ),  
                    rootCons.getCdr()));
            
            //          proc.setNameList( Arrays.asList(symbols) );
        }
    }

    public Object defineDoc( Environment env, DescriptiveBean bean ) {
        return defineDoc0( env, this, bean.getName(), null,   bean );
    }
    public Object defineDoc( Environment env, String targetVar, DescriptiveBean bean ) {
        return defineDoc0( env, this, targetVar,      null,   bean );
    }
    public Object defineDoc( Environment env, Object target, DescriptiveBean bean ) {
        return defineDoc0( env, this, null,           target, bean );
    }
    public static Object defineDoc0(Environment env, DescriptiveDocumentCategory type, String targetVar, Object target, DescriptiveBean bean ) {
        synchronized ( Language.getDefaultLanguage() ) {
            Object actualTarget = defineDoc0( env, type, targetVar, target, bean.format(), bean.getNames() );
            Descriptive.setDescriptionBean( actualTarget, bean );
            return actualTarget;
        }
    }

    //      static Procedure proc_defineDocument = eval( lis( "lambda", lis("rt"),   ) );   
    public static Object defineDoc0( Environment env, DescriptiveDocumentCategory type, final String targetVar, final Object target, String description, List<String> names )  {
        if ( DEBUG )
            logInfo( "DescriptiveDocumentType.defineDoc0()" + targetVar );
        Object actualTarget;
        if ( target != null ) {
            actualTarget = target;
        } else {
            if ( targetVar == null ) {
                throw new IllegalArgumentException( "targetVar cannot be null when target is null." );
            }
            actualTarget = SchemeUtils.getVar( targetVar, null );
            if ( actualTarget == null ) {
                if ( DEBUG )
                    Descriptive.logWarn( "setDocumentInitializer: " + targetVar + " was not found." );
                actualTarget = new DescriptiveHelpProcedure( targetVar );
                SchemeUtils.defineVar( env, actualTarget, targetVar );
            }
        }
            
        if ( DEBUG )
            logInfo( "setting description on '" + targetVar + "'" + " " + actualTarget.toString() );
        //          logInfo( "description" );
        //          logInfo( description );
        Descriptive.setDescription( actualTarget, description );
        addDocumentList(
            env,
            type,
            SchemeUtils.stringListToSymbolList( names ), 
            actualTarget );
        
        return actualTarget;
    }
    

    public static void outputAvailableReferences(String outputFile) throws FileNotFoundException, IOException {
        List<String> stringList = new ArrayList<>();
        for ( DescriptiveDocumentCategory c : getAll() ) {
            stringList.add( SchemeUtils.schemeStringToJavaString( c.getSymbol() ) );
        }
        String str = String.join( ",", stringList );
        if ( outputFile == null /* || "-".equals( outputFile ) */ ) {
            System.out.println( str );      
        } else {
            FileOutputStream fo = null;
            try {
                fo = new FileOutputStream( new File( outputFile ) );
                fo.write( str.getBytes( Charset.forName( "utf-8" ) ) );
                System.err.println( str );      
                fo.flush();
            } finally {
                if ( fo != null )
                    fo.close();
            }
        }
    }

    public static void outputReference(Environment environment, String categoryName, String outputFile)
            throws FileNotFoundException, IOException 
    {
        DescriptiveDocumentCategory category = valueOf( categoryName );
        String str = DescriptiveHelp.outputMarkdownReference( category, environment );
        if ( outputFile == null /* || "-".equals( outputFile ) */ ) {
            System.out.println( str );      
        } else {
            FileOutputStream fo = null;
            try {
                fo = new FileOutputStream( new File( outputFile ) );
                fo.write( str.getBytes( Charset.forName( "utf-8" ) ) );
                System.err.println( str );      
                fo.flush();
            } finally {
                if ( fo != null )
                    fo.close();
            }
        }
    }
}
