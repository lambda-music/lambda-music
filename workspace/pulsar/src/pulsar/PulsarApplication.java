package pulsar;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import kawapad.KawapadDocuments;
import pulsar.lib.GC;
import pulsar.lib.PulsarLogger;
import pulsar.lib.Version;
import pulsar.lib.app.ApplicationComponent;
import pulsar.lib.app.ApplicationVessel;
import pulsar.lib.scheme.SchemeEngine;
import pulsar.lib.scheme.doc.DescriptiveDocumentCategory;
import pulsar.lib.scheme.doc.DescriptiveHelp;
import pulsar.lib.scheme.http.SchemeHttp;

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
    static List<ApplicationComponent> parseArgs01( String[] args ) throws IOException {
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
    static List<ApplicationComponent> parseArgs02( String[] args ) throws IOException {
        List<ApplicationComponent> applicationVesselList = new ArrayList<>();
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
                    applicationVesselList.addAll( pulsarApplicationArgumentParser.getApplicationVesselList() );
                } else {
                    throw new RuntimeException( "unknown command " + mainCommand );
                }
            } else {
                throw new RuntimeException( "" );
            }
        }
        return applicationVesselList;
    }
    
    static void remote(String[] mainArguments) {
        // TODO
    }
    private static void invalidArgs() {
        System.err.println( "pulsar : missing arguments." );
        System.err.println( "pulsar [scheme|pulsar|http|gui|token|print-all-available-reference|print-reference] ... " );
    }
    
    static List<ApplicationComponent> parseArgs( String[] args ) throws IOException {
        if ( args.length == 0 ) {
            invalidArgs();
        }
        if ( args[0].equals( "advanced" ) ) {
            return parseArgs02( Arrays.copyOfRange( args , 1, args.length ) );
        } else {
            List<ApplicationComponent> list = parseArgs01( args );
            ApplicationVessel applicationVessel = new ApplicationVessel();
            applicationVessel.addAll( list );
            return Arrays.asList( (ApplicationComponent)applicationVessel );
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
    
    static Pulsar lookupPulsar(List<ApplicationComponent> list ) {
        for ( ApplicationComponent c : list ) {
            if ( c instanceof Pulsar ) {
                return (Pulsar) c;
            }
        }
        return null;
    }
    static List<ApplicationComponent> outputAllAvailableReferences( String outputFile ) throws IOException {
        List<ApplicationComponent> list = start(true,true,8193);
        Pulsar pulsar = lookupPulsar( list );
        loadAllAvailableHelps();
        DescriptiveDocumentCategory.outputAvailableReferences( outputFile );
        quitPulsarSafely( pulsar );
        return list;
    }
    
    static List<ApplicationComponent>  outputReference( String outputFile, String categoryName ) throws IOException {
        loadAllAvailableHelps();
        List<ApplicationComponent> list = start( true, true, 8193 );
        Pulsar pulsar = lookupPulsar( list );
        DescriptiveDocumentCategory.outputReference( pulsar.getSchemeEngine(), categoryName, outputFile );
        quitPulsarSafely( pulsar );
        return list;
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
        pulsar.getParentApplicationComponent().processQuit();

        // This is possibly not necessary. But it might help to flush the AWT-eventqueue.
        // See https://stackoverflow.com/questions/6309407/remove-top-level-container-on-runtime
        GC.exec();
    }
    
    public static List<ApplicationComponent> start( boolean guiEnabled, boolean httpEnabled, int httpPort, String filename ) throws IOException {
        SchemeEngine schemeEngine = PulsarApplicationLibrary.createSchemeEngine();
        Pulsar pulsar = PulsarApplicationLibrary.createPulsar( schemeEngine );
        PulsarFrame pulsarFrame;
        if ( guiEnabled ) {
            pulsarFrame = PulsarApplicationLibrary.createPulsarGui( 
                schemeEngine, pulsar, 
                Arrays.asList( "http://localhost:"+httpPort+"/eval" ) );
        } else {
            pulsarFrame = null;
        }
        
        SchemeHttp schemeHttp = null;
        if ( httpEnabled ) {
            schemeHttp = PulsarApplicationLibrary.createPulsarHttpServer( schemeEngine, httpPort, SchemeHttp.UserAuthentication.ONLY_LOOPBACK, pulsar );
        }
        
        schemeEngine.getSchemeEvaluator().newScheme();
        
        if ( pulsarFrame != null ) {
            pulsarFrame.processInit();
            
            if ( filename != null ) {
                pulsarFrame.openFile( new File( filename ) );
            } else {
                pulsarFrame.openIntro();
            }
        }
        ArrayList<ApplicationComponent> result = new ArrayList<>();
        if ( schemeEngine != null )
            result.add( schemeEngine );
        if ( pulsar != null )
            result.add( pulsar );
        if ( pulsarFrame != null )
            result.add( pulsarFrame );
        if ( schemeHttp != null )
            result.add( schemeHttp );
        
        return result;
    }
    public static List<ApplicationComponent> start(boolean guiEnabled, boolean httpEnabled, int httpPort ) throws IOException {
        return start(guiEnabled, httpEnabled, httpPort, null );
    }
    

    public static void main(String[] args) throws IOException {
        System.err.println( "*** WELCOME TO PULSAR ***" );
        System.err.println( "VERSION : " + Version.get( PulsarApplication.class ) );
        PulsarLogger.init();
        PulsarPrinter.init();
//        parseArgs01(args);
        
        List<ApplicationComponent> parseArgs = parseArgs(args);

        Thread thread = new Thread( new Runnable() {
            @Override
            public void run() {
                BufferedReader r = new BufferedReader( new InputStreamReader( System.in ) );
                try {
                    try {
                        for (;;) {
                            String s = r.readLine();
                            if ( s == null ) 
                                break;
                            if (   "quit".equals( s ) 
                                || "bye" .equals( s ) 
                                    ) {
                                System.out.println( "ok" );
                                for ( ApplicationComponent c : parseArgs ) {
                                    try {
                                        c.processQuit();
                                    } catch ( Throwable t ) {
                                        logError( "", t );
                                    }
                                }
                                break;
                            } else if ( "alive?".equals( s ) ) {
                                System.out.println( "yes" );
                            } else if ( "hello".equals( s ) ) {
                                System.out.println( "hello" );
                            } else {
                                System.out.println( "unknown-command" );
                            }
                        }
                    } finally {
                        r.close();
                    }
                } catch ( IOException e ) {
                    logError( "", e );
                }
            }
        }, "command-reception" );
        thread.setDaemon( true );
        thread.start();
    }
}
