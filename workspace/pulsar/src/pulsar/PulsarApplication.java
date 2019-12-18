package pulsar;

import java.io.File;
import java.io.IOException;
import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.logging.Level;
import java.util.logging.Logger;

import kawapad.KawapadDocuments;
import pulsar.lib.GC;
import pulsar.lib.PulsarLogger;
import pulsar.lib.Version;
import pulsar.lib.scheme.DescriptiveDocumentCategory;
import pulsar.lib.scheme.DescriptiveHelp;
import pulsar.lib.scheme.http.SchemeHttp;
import pulsar.lib.scheme.scretary.SchemeSecretary;

public class PulsarApplication {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }
    public PulsarApplication() {
    }
    /**
     * The main method which starts up the application. The application opens a file
     * which is specified in argument values. When more than one arguments were
     * passed to the method, only the first argument is taken. 
     * @throws IOException 
     */
    static Pulsar parseArgs01( String[] args ) throws IOException {
        boolean argSpecialOutputReference = false;
        String  argSpecialOutputReferenceName = null;
        boolean argSpecialAllAvailableReferences = false;
        boolean argHttp = true;
        int     argHttpPort = 8192;
        boolean argGui = true;
        String  argFileName = null;
        for ( int i=0; i<args.length; i++ ) {
            String s = args[i];
            
            
            if ( s.equals( "--version" ) ) {
                System.out.println( Version.get( PulsarApplication.class ) );
                return null;
            } else if ( s.startsWith( "--http=" ) ) {
                argHttp = true;
                argHttpPort = Integer.parseInt( s.substring( "--http=".length() ) );
                break;
            } else if ( s.equals( "--http" ) ) { 
                argHttp = true;
                argHttpPort = 8192;
                break;
            } else if ( s.equals( "--gui" ) ) { 
                argGui = true;
                break;
            } else if ( s.equals( "--no-http" ) ) { 
                argHttp = false;
                break;
            } else if ( s.equals( "--no-gui" ) ) { 
                argGui = false;
                break;
            } else if ( s.equals( "--all-available-references" ) ) { 
                argSpecialAllAvailableReferences = true;
                break;
            } else if ( s.equals( "--output-reference" ) ) { 
                argSpecialOutputReference = true;
                i++;
                if ( i < args.length  ){
                    argSpecialOutputReferenceName = args[i];
                    System.err.println( "--output-reference:" + argSpecialOutputReferenceName  );
                } else {
                    System.err.println( "warning : insufficient nubmer of arguments."
                            + " The given argument --output-reference was ignroed." );  
                }
                break;
            } else {
                if ( argFileName == null )
                    argFileName = s;
            }
        }

        if ( false ) {
            // dummy
            return null; 
        } else if ( argSpecialAllAvailableReferences ) {
            return outputAllAvailableReferences( argFileName );
        } else if ( argSpecialOutputReference ) {
            return outputReference( argFileName,  argSpecialOutputReferenceName );
        } else if ( argHttp || argGui ) {
            return start(argGui, argHttp, argHttpPort, argFileName);
        } else {
            System.err.println( "hmm... you have better to have at least one interface to control the system." );
            return start(argGui, argHttp, argHttpPort, argFileName);
        }
    }
    static <T> int indexOf( T[] a, T v ) {
        for ( int i=0; i<a.length; i++ ) {
            if ( v == a[i] || v.equals( a[i] ) )
                return i;
        }
        return -1;
    }
    static <T> T[][] splitArray(T[] a, T separator ) {
        ArrayList<T[]> result = new ArrayList<>();
        int last = 0;
        for(;;){
            int i = indexOf( a, separator );
            if ( i < 0 )
                break;
            if ( last != i ) 
                result.add( Arrays.copyOfRange( a, last , i ) );
            last = i+1;
        }
        if ( result.size() == 0 ) {
            result.add( a.clone() );
        }
        return result.toArray((T[][])java.lang.reflect.Array.newInstance( a.getClass(), result.size() ) );
    }
    static void parseArgs02( String[] args ) throws IOException {
        String[][] args2 = splitArray( args, "," ) ;
        for ( int i=0; i<args2.length; i++ ) {
            String[] args3 = args2[i];
            if ( 0 < args3.length ) {
                String mainCommand = args3[0];
                String[] mainArguments = Arrays.copyOfRange( args3 , 1, args3.length );
                
                if( "fork".equals( mainCommand ) ) {
                    remote( mainArguments );
                } else if( "exec".equals( mainCommand ) ) {
                    PulsarApplicationArgumentParser pulsarApplicationArgumentParser = new PulsarApplicationArgumentParser();
                    pulsarApplicationArgumentParser.parse( mainArguments );
                } else {
                    throw new RuntimeException( "unknown command " + mainCommand );
                }
            } else {
                throw new RuntimeException( "" );
            }
        }
    }
    
    static void remote(String[] mainArguments) {
        // TODO
    }
    private static void invalidArgs() {
        System.err.println( "pulsar : missing arguments." );
        System.err.println( "pulsar [scheme|pulsar|http|gui|token|print-all-available-reference|print-reference] ... " );
    }
    
    static void parseArgs( String[] args ) throws IOException {
        if ( args.length == 0 ) {
            invalidArgs();
        }
        if ( args[0].equals( "advanced" ) ) {
            parseArgs02( Arrays.copyOfRange( args , 1, args.length ) );
        } else {
            parseArgs01( args );
        }
    }

    private static void forceLoad( Class c ) {
        try {
            Class.forName( c.getName(), true, c.getClassLoader() );
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        }
    }
    
    private static void loadAllAvailableHelps() {
        forceLoad( PulsarDocuments.class );
        forceLoad( PulsarFramePackage.class );
        forceLoad( KawapadDocuments.class );
        forceLoad( DescriptiveHelp.class );
    }
    
    static Pulsar outputAllAvailableReferences( String outputFile ) throws IOException {
        Pulsar pulsar = start(true,true,8193);
        loadAllAvailableHelps();
        DescriptiveDocumentCategory.outputAvailableReferences( outputFile );
        quitPulsarSafely( pulsar );
        return pulsar;
    }
    
    static Pulsar outputReference( String outputFile, String categoryName ) throws IOException {
        loadAllAvailableHelps();
        Pulsar pulsar = start( true, true, 8193 );
        DescriptiveDocumentCategory.outputReference( pulsar.getSchemeSecretary(), categoryName, outputFile );
        quitPulsarSafely( pulsar );
        return pulsar;
    }
    static void quitPulsarSafely(Pulsar pulsar) {
        // Pause for the safe shutdown; at the moment when it runs here, other initializers are still
        // working and they somehow remain on the AWT-eventqueue and prevent JVM to
        // shutdown. (Mon, 26 Aug 2019 14:11:31 +0900)
        try {
            Thread.sleep( 2048 );
        } catch (InterruptedException e) {
            System.err.println( e.getMessage() );
        }
        pulsar.invokeLater( new Runnable() {
            @Override
            public void run() {
                pulsar.quit();
            }
        });
        // This is possibly not necessary. But it might help to flush the AWT-eventqueue.
        // See https://stackoverflow.com/questions/6309407/remove-top-level-container-on-runtime
        GC.exec();
    }
    
    public static Pulsar start( boolean guiEnabled, boolean httpEnabled, int httpPort, String filename ) throws IOException {
        SchemeSecretary schemeSecretary = PulsarApplicationLibrary.createSchemeSecretary();
        Pulsar pulsar = PulsarApplicationLibrary.createPulsar( schemeSecretary );
        PulsarFrame pulsarFrame;
        if ( guiEnabled ) {
            pulsarFrame = PulsarApplicationLibrary.createPulsarGui( schemeSecretary, pulsar, "http://localhost:"+httpPort+"/eval" );
        } else {
            pulsarFrame = null;
        }
        
        if ( httpEnabled ) {
            PulsarApplicationLibrary.createPulsarHttpServer( schemeSecretary, httpPort, SchemeHttp.UserAuthentication.ONLY_LOOPBACK, pulsar );
        }
        
        schemeSecretary.newScheme();
        
        if ( pulsarFrame != null ) {
            pulsarFrame.init();
            
            if ( filename != null ) {
                pulsarFrame.openFile( new File( filename ) );
            } else {
                pulsarFrame.openIntro();
            }
        }
        
        return pulsar;
    }
    public static Pulsar start(boolean guiEnabled, boolean httpEnabled, int httpPort ) throws IOException {
        return start(guiEnabled, httpEnabled, httpPort, null );
    }
    

    public static void main(String[] args) throws IOException {
        System.err.println( "*** WELCOME TO PULSAR ***" );
        System.err.println( "VERSION : " + Version.get( PulsarApplication.class ) );
        PulsarLogger.init();
        PulsarPrinter.init();
//        parseArgs01(args);
        parseArgs(args);
    }
}
