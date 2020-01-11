package pulsar;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.logging.Level;

import kawapad.KawapadDocuments;
import pulsar.lib.PulsarLogFormatter;
import pulsar.lib.Version;
import pulsar.lib.app.ApplicationComponent;
import pulsar.lib.app.ApplicationVessel;
import pulsar.lib.app.process.JavaProcess;
import pulsar.lib.log.PulsarLogger;
import pulsar.lib.scheme.doc.DescriptiveHelp;

public class PulsarApplication {
    static final PulsarLogger LOGGER = PulsarLogger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }
    private PulsarApplication() {
    }
    
    static List<ApplicationComponent> parseArgs( String[] args ) throws IOException {
        PulsarApplicationArgumentMacro macro = new PulsarApplicationArgumentMacro( Arrays.asList( args ) );
        macro.exec();
        args = macro.getOutputAsArray();
        System.err.println( macro.getOutput() );
        List<ApplicationComponent> vessels = new ArrayList<>();
        String[][] arrayOfArgs = PulsarApplicationArraySplitter.splitBeginEnd( args, "begin", "end" ) ;
        
        for ( int i=0; i<arrayOfArgs.length; i++ ) {
            List<String> argguments = new ArrayList<>( Arrays.asList( arrayOfArgs[i] ));
            if ( 0 < argguments.size() ) {
                String mainCommand    = argguments.remove( 0 );
                
                if( "fork".equals( mainCommand ) ) {
                    // fork
                    vessels.add( forkPulsar( argguments ) );
                    
                } else if( "exec".equals( mainCommand ) ) {
                    // exec
                    PulsarApplicationArgumentParser argumentParser = new PulsarApplicationArgumentParser();
                    argumentParser.parse( argguments );
                    vessels.addAll( argumentParser.getApplicationVesselList() );
                } else {
                    throw new RuntimeException( "unknown command " + mainCommand );
                }
            } else {
                throw new RuntimeException( "" );
            }
        }
        return vessels;
    }
    
    static JavaProcess forkPulsar(List<String> arguments) {
        JavaProcess process = new JavaProcess( 
            PulsarApplication.class.getCanonicalName(), arguments );
        return process;
    }
    
    private static void forceLoad( Class c ) {
        try {
            Class.forName( c.getName(), true, c.getClassLoader() );
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        }
    }
    
    static void loadAllAvailableHelps() {
        forceLoad( PulsarDocuments.class );
        forceLoad( PulsarFramePackage.class );
        forceLoad( KawapadDocuments.class );
        forceLoad( DescriptiveHelp.class );
    }

    private static void invalidArgs() {
        System.err.println( "pulsar : missing arguments." );
        System.err.println( "pulsar [scheme|pulsar|http|gui|token|print-all-available-reference|print-reference] ... " );
    }
    
    
    public static void main(String[] args) throws IOException {
        System.err.println( "*** WELCOME TO PULSAR ***" );
        System.err.println( "VERSION : " + Version.get( PulsarApplication.class ) );
        PulsarLogFormatter.init();
        PulsarPrinter.init();
        
        if ( args.length == 0 ) {
            // TODO
            invalidArgs();
            return ;
        }

        List<ApplicationComponent> components = parseArgs(args);
        
        ApplicationVessel owner = new ApplicationVessel();
        owner.addAll( components );
        owner.requestInit();
        
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
                                owner.processQuit();
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
