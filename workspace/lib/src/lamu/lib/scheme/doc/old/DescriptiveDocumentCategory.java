package lamu.lib.scheme.doc.old;	

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
    private static final Environment environment = Environment.getInstance( DescriptiveDocumentCategory.class.getCanonicalName() );
        
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
    public static synchronized void addCategory( DescriptiveDocumentCategory category ) {
        allCategories.put( category.symbol, category );
    }
    
    /**
     * Removes the specified category.
     * @param category
     * the category object to be removed.
     */
    public static synchronized void removeCategory( DescriptiveDocumentCategory category ) {
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

    public synchronized static DescriptiveDocumentCategory valueOf( Symbol symbol ) {
        return allCategories.get( symbol );
    }
    public synchronized static DescriptiveDocumentCategory valueOf( String symbolString ) {
        return allCategories.get( Symbol.valueOf( symbolString ) );
    }

    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE,   msg, e   ); }
    static void logInfo (String msg             ) { LOGGER.log(Level.INFO,     msg      ); }
    static void logWarn (String msg             ) { LOGGER.log(Level.WARNING,  msg      ); }

    /**
     * Returns the cons cell which is corresponding to the specified symbol. 
     * <p>
     * <b>Caution</b> (Tue, 07 Apr 2020 12:42:15 +0900)
     * <p>
     * This method used to be a way to retrieve the common cons cell
     * from the passed environment object; now it ignores the passed environment
     * and take the cons cell from the common environment object which this class
     * independently manages.
     * <p>
     * Because every procedure object used to be created for each environment object;
     * in the modification on (Tue, 07 Apr 2020 12:42:15 +0900) every procedure 
     * should be created for each JVM instance. Now procedures are not allowed to be
     * created twice or more at a JVM session.
     * <p>
     * @param symbol
     *     The symbol which is related to the cons cell.
     * 
     * @return
     *     The cons cell.
     */
    private static Pair getRootConsCell( Symbol symbol ) {
        Environment env = environment;

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
    public LList getDocumentList() {
        return getDocumentList( this );
    }
    private static LList getDocumentList( DescriptiveDocumentCategory type ) {
        synchronized ( Language.getDefaultLanguage() ) {
            Pair rootCons = getRootConsCell( type.getSymbol() );
            return (LList) rootCons.getCdr();
        }
    }

    private static void addDocumentList( DescriptiveDocumentCategory type, DescriptiveBean bean ) {
        synchronized ( Language.getDefaultLanguage() ) {
            Pair rootCons = getRootConsCell( type.getSymbol() );
            rootCons.setCdr( 
                Pair.make(
                    Pair.make( bean, 
                        LList.makeList(
                            SchemeUtils.stringListToSymbolList( bean.getNames() ), 0 ) ),  
                    rootCons.getCdr()));
        }
    }

    public void defineDoc( DescriptiveBean bean ) {
        synchronized ( Language.getDefaultLanguage() ) {
            addDocumentList( this, bean );
        }
    }
    public void defineDoc( Environment env, String targetVar, DescriptiveBean bean ) {
        synchronized ( Language.getDefaultLanguage() ) {
            addDocumentList( this, bean );
        }
    }
    public void defineDoc( Environment env, Object target, DescriptiveBean bean ) {
        synchronized ( Language.getDefaultLanguage() ) {
            addDocumentList( this, bean );
        }
    }
    public static void initDoc( DescriptiveDocumentCategory category, Object target, DescriptiveBean bean ) {
        DescriptiveProcedure.setDescriptionBean( target, bean );
        DescriptiveProcedure.setDescription( target, bean.format() );
        synchronized ( Language.getDefaultLanguage() ) {
            addDocumentList( category, bean );
        }
    }

    /**
     * 
     * This outputs the formatted documentation 
     * @param outputFile
     *     the file name to be output.
     *     
     * @throws IOException
     */
    public static void outputAvailableReferences( String outputFile ) throws IOException {
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
