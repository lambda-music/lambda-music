package lamu.lib.scheme.doc;	

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
import lamu.lib.log.Logger;
import lamu.lib.scheme.SchemeUtils;

/**
 * 
 * 1. Note that the
 * {@link DescriptiveDocumentCategory#createCategory(Symbol, Runnable)} method
 * will not be called unless the caller class is loaded by VM and the loading
 * process will not be done unless the user does not causes the class to be
 * loaded. Please read {@link lamu.LamuApplication#loadBasicClasses}.
 * 
 * 2. The documentation object will not properly be initialized unless its host
 * object is instanciated; especially the new mechanism that loads the libraries
 * by KawaScheme's `require` method needs extra procedures to initialize its
 * corresponding document. Because the documentation is not initalized unless
 * the library object is properly loaded by `require` method. This will end up
 * the documentation system outputs an empty file because at that point, the
 * document object is not created yet.
 * 
 * This is where `Runnable documentInitializer` parameter of createCategory() method
 * comes in. The initializer object creates the document object instead of
 * `require` method. This note will be deleted if all objects migrate to the new
 * mechanism to load the library.
 * 
 * @author ats
 *
 */

public class DescriptiveDocumentCategory {
    public static DescriptiveDocumentCategory createCategory(Symbol symbol, Runnable documentInitializer ) {
		return new DescriptiveDocumentCategory(symbol, documentInitializer);
    }
    public static DescriptiveDocumentCategory createCategory(String symbol, Runnable documentInitializer ) {
		return new DescriptiveDocumentCategory(SchemeUtils.schemeSymbol(symbol), documentInitializer);
    }
    static final Map<Symbol,DescriptiveDocumentCategory> allCategories = new LinkedHashMap<>();

    /**
     * Adds the specified category.
     * @param category
     * the category object to be added.
     */
    public static synchronized void addCategory(DescriptiveDocumentCategory category) {
        allCategories.put( category.symbol, category );
    }
    
    /**
     * Removes the specified category.
     * @param category
     * the category object to be removed.
     */
    public static synchronized void removeCategory(DescriptiveDocumentCategory category) {
        allCategories.remove( category.symbol );
    }
    
    /**
     * This procedure returns a list which contains all available DocumentCategory objects.
	 * Note that this method only returns the category objects which are already loaded to
	 * the current virtual machine. In order to get the specific document category, the class
	 * which is related the category should be loaded before calling this method.
	 * 
	 * @see {@link LamuApplication#loadBasicClasses }
	 * 
     * @return
     *    an {@link ArrayList} object which contains all available {@link DescriptiveDocumentCategory} objects.
     */
    public static synchronized Collection<DescriptiveDocumentCategory> getAllCategories() {
        return new ArrayList<>( allCategories.values() );
    }

    private static final boolean DEBUG = false; 
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
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
	private Runnable documentInitializer;
    private DescriptiveDocumentCategory( Symbol symbol, Runnable documentInitializer ) {
        this.symbol = symbol;
        this.documentInitializer = documentInitializer;
        DescriptiveDocumentCategory.addCategory( this );
    }
    public Symbol getSymbol() {
        return symbol;
    }
    public void executeDocumentInitializer() {
    	this.documentInitializer.run();
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
    

    /**
     * 
     * This outputs the formatted documentation 
     * @param outputFile
     *     the file name to be output.
     *     
     * @throws IOException
     */
    public static void outputAvailableReferences(String outputFile) throws IOException {
        List<String> stringList = new ArrayList<>();
        for ( DescriptiveDocumentCategory c : getAllCategories() ) {
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
        
        // ADDED (Sun, 08 Mar 2020 00:59:17 +0900) >>>
        // Call its initializer to invoke effectively the initScheme() method; this
        // causes the document object to be initialized.  
        Environment.setCurrent( environment );
        category.executeDocumentInitializer();
        // ADDED (Sun, 08 Mar 2020 00:59:17 +0900) <<<
        
        // Note (Sun, 08 Mar 2020 01:02:03 +0900) 
        // This initialization process is related to the way how these libraries are initialized in
        // the current system. These initScheme() based initialization is not effective; these should be
        // replaced to the new KawaScheme's `require` method.
        
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
