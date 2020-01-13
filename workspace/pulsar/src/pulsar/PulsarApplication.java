package pulsar;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.logging.Level;

import kawapad.Kawapad;
import kawapad.KawapadDocuments;
import pulsar.PulsarApplicationDefaultArgument.Element;
import pulsar.lib.PulsarLogFormatter;
import pulsar.lib.Version;
import pulsar.lib.app.ApplicationComponent;
import pulsar.lib.app.ApplicationVessel;
import pulsar.lib.app.process.JavaProcess;
import pulsar.lib.log.PulsarLogger;
import pulsar.lib.scheme.doc.DescriptiveHelp;
import pulsar.lib.swing.PulsarGuiUtils;

public class PulsarApplication {
    static final PulsarLogger LOGGER = PulsarLogger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }
    private PulsarApplication() {
    }

    private static void exec1( List<ApplicationComponent> vessels, List<String> arguments ) throws IOException {
        String mainCommand;
        if ( 0 < arguments.size() ) {
            mainCommand = arguments.get( 0 );
        } else {
            mainCommand = null;
        }

        if( "fork".equals( mainCommand ) ) {
            List<String> subArguments = arguments.subList( 1, arguments.size() );
            // fork
            vessels.add( forkPulsar( subArguments ) );
            
        } else if( "exec".equals( mainCommand ) ) {
            List<String> subArguments = arguments.subList( 1, arguments.size() );
            // exec
            PulsarApplicationArgumentParser argumentParser = new PulsarApplicationArgumentParser();
            argumentParser.parse( subArguments );
            vessels.addAll( argumentParser.getApplicationVesselList() );
        } else {
            List<Element> defaultArgumentList = PulsarApplicationDefaultArgument.load();
            Element defaultArgument = null;
            if ( defaultArgumentList.isEmpty() ) {
                defaultArgument = new Element( "default", "exec scheme + pulsar + pulsar-gui $* +" );
            } else {
                defaultArgument = defaultArgumentList.get( 0 );
            }
            
            List<String> subArguments = defaultArgument.interpolate( String.join( " ", arguments ) );
            // a recursive calling
            exec1( vessels, subArguments );
        }
    }

    static List<ApplicationComponent> parseArgs( String[] args ) throws IOException {
        {
            PulsarApplicationArgumentMacro macro = new PulsarApplicationArgumentMacro( Arrays.asList( args ) );
            macro.exec();
            args = macro.getOutputAsArray();
            System.err.println( macro.getOutput() );
        }

        String[][] arrayOfArgs = PulsarApplicationArraySplitter.splitBeginEnd( args, "begin", "end" ) ;
        
        List<ApplicationComponent> vessels = new ArrayList<>();
        if ( arrayOfArgs.length == 0 ) {
            exec1( vessels, Collections.emptyList() );
        } else {
            for ( int i=0; i<arrayOfArgs.length; i++ ) {
                List<String> arguments = new ArrayList<>( Arrays.asList( arrayOfArgs[i] ));
                exec1( vessels, arguments );
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
    
    static void loadBasicClasses() {
        // For documentation.
        forceLoad( PulsarDocuments.class );
        forceLoad( PulsarGuiUtils.class );
        forceLoad( KawapadDocuments.class );
        forceLoad( DescriptiveHelp.class );
        
        // See those static blocks.
        forceLoad( Kawapad.class );
        forceLoad( Pulsar.class );
    }
    
    static void initKawaImportPath() {
        String value = System.getProperty( "kawa.import.path" );
        if ( value == null ) 
            value = "";
        else
            value = value + ":";
        
        String homeValue = System.getProperty( "user.home" );
        if ( homeValue != null ) {
            value = value + 
                    homeValue + ".pulsar/"  + ":" +
                    homeValue + ".kawapad/" + "";
            System.setProperty( "kawa.import.path", value );
        } else {
            // do nothing
        }
    }

    public static void main(String[] args) throws IOException {
        // Initialize Kawa import path in the first place.
        initKawaImportPath();
        
        // This causes invoking various initialization procedures.
        loadBasicClasses();
        
        
        System.err.println( "*** WELCOME TO PULSAR ***" );
        System.err.println( "VERSION : " + Version.get( PulsarApplication.class ) );
        PulsarLogFormatter.init();
        PulsarPrinter.init();

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
