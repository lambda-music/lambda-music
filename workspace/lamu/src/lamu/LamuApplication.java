package lamu;

import java.io.File;
import java.io.IOException;
import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;

import kawapad.Kawapad;
import kawapad.KawapadTextualIncrement;
import lamu.lib.ForceLoadingClass;
import lamu.lib.InstanceManager;
import lamu.lib.Version;
import lamu.lib.app.ApplicationComponent;
import lamu.lib.app.ApplicationVessel;
import lamu.lib.app.args.ArgsCommand;
import lamu.lib.app.args.ArgsCommandEcho;
import lamu.lib.app.args.ArgsCommandExec;
import lamu.lib.app.args.ArgsCommandFork;
import lamu.lib.app.args.ArgsCommandLoad;
import lamu.lib.app.args.ArgsCommandMacro;
import lamu.lib.app.args.ArgsCommandState;
import lamu.lib.doc.LamuDocument;
import lamu.lib.log.Logger;
import lamu.lib.scheme.SchemeEngineLib;
import lamu.lib.scheme.SchemeEvaluatorLib;
import lamu.lib.scheme.repl.SimpleReplService;
import lamu.lib.stream.SisoReceiver;
import lamu.lib.stream.StdioStream;
import lamu.utils.lib.PulsarGuiUtils;
import pulsar.Pulsar;
import pulsar.PulsarLib;
import pulsar.PulsarLib_Notes;
import pulsar.PulsarLib_Procs;

public class LamuApplication {
    // (Sun, 29 Mar 2020 23:16:11 +0900)
    static {
        readEnvironmentToSystemPropertyMap();
        initLogger();
    }

    static final Logger LOGGER = Logger.getLogger(MethodHandles.lookup().lookupClass().getName());
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg) { LOGGER.log(Level.INFO, msg); }
    static void logWarn(String msg) { LOGGER.log(Level.WARNING, msg); }
    private LamuApplication() {
    }

    static File getInitFile() {
        return new File( System.getProperty("user.home"), ".lamu/default-arguments.conf");
    }
    
    static List<ArgsCommand> createAvailableCommandList() throws IOException {
        List<ArgsCommand> availableCommands = new ArrayList<>();
        availableCommands.add( new LamuCommandBuilder() );
        availableCommands.add( new ArgsCommandFork( "lamu", LamuApplication.class ) );
        availableCommands.add( new ArgsCommandLoad() );
        availableCommands.add( new ArgsCommandExec() );
        availableCommands.add( new ArgsCommandEcho() );
        availableCommands.addAll( ArgsCommandMacro.load( getInitFile() ) );

        // this is a fall back.
        availableCommands.add( ArgsCommandMacro.create( 
                    LamuScript.DEFAULT_COMMAND_LOAD + " " + 
                    " create scheme + pulsar + repl $*{--load=$} +" ));

        availableCommands.add( ArgsCommandMacro.create( 
                    LamuScript.DEFAULT_COMMAND_OPEN + " " + 
                    " create scheme + pulsar + repl + gui $*{$} +" ));
        
        availableCommands.add( ArgsCommandMacro.create( 
            LamuScript.DEFAULT_COMMAND + " " + 
            LamuScript.DEFAULT_COMMAND_OPEN + " $*{$}" ));

        return availableCommands;
    }


    static void loadBasicClasses() {
        // For documentation.
        ForceLoadingClass.force(PulsarLib_Notes.class);
        ForceLoadingClass.force(PulsarGuiUtils.class);
        ForceLoadingClass.force(PulsarLib_Procs.class);
        ForceLoadingClass.force(PulsarLib_Notes.class);
        ForceLoadingClass.force(PulsarLib.PulsarLibImplementation.class);
        ForceLoadingClass.force(PulsarLib.class);
        ForceLoadingClass.force(lamu.help.class );
        ForceLoadingClass.force(lamu.scheme.class );
        
        ForceLoadingClass.force(SchemeEngineLib.class);
        ForceLoadingClass.force(SchemeEvaluatorLib.class);

        // See those static blocks.
        ForceLoadingClass.force(Kawapad.class);
        ForceLoadingClass.force(KawapadTextualIncrement.class);
        ForceLoadingClass.force(Pulsar.class);
        ForceLoadingClass.force(PulsarLib_Procs.class);
        
        LamuDocument.debugOut();
    }

    static void initKawaImportPath() {
        String value = System.getProperty("kawa.import.path");
        if (value == null)
            value = "";
        else
            value = value + ":";

        String homeValue = System.getProperty("user.home");
        if (homeValue != null) {
            value = value + homeValue + ".pulsar/" + ":" + homeValue + ".kawapad/" + "";
            System.setProperty("kawa.import.path", value);
        } else {
            // do nothing
        }
    }

    private static final class InstanceManagerComponent implements ApplicationComponent {
        @Override
        public void processInit() {
        }

        @Override
        public void processQuit() {
            InstanceManager.shutdown();
        }

        ApplicationComponent parent= null;
        @Override
        public void setParentApplicationComponent(ApplicationComponent parent) {
            this.parent = parent;
        }
        @Override
        public ApplicationComponent getParentApplicationComponent() {
            return parent;
        }
    }


    /**
     * If no reception object exists in the component list, create a default
     * reception object and add to the list. A reception object is the server
     * to receive/send via stdin/stdout.
     * 
     * Right now there is only one kind of reception objects; SisoReceiver; though
     * it will be changed. 
     * 
     * And this spec is currently disabled. (Fri, 20 Mar 2020 21:43:54 +0900)
     */
    static void checkRepl( ApplicationVessel vessel ) {
        boolean found = false;
        List<ApplicationComponent> components = vessel.getComponents();
        for ( Iterator<ApplicationComponent> i= components.iterator();i.hasNext(); ) {
            ApplicationComponent c = i.next();
            if ( c instanceof SisoReceiver ) {
                found = true;
            }
        }
        if ( ! found ) {
            //              Thread thread = new Thread( new LamuSimpleSocketServer( owner, System.in, System.out), "command-reception" );
            //              thread.setDaemon(true);
            //              thread.start();
            vessel.add( new SisoReceiver( StdioStream.INSTANCE, new SimpleReplService() ) );
        }
    }

    static void readEnvironmentToSystemPropertyMap() {
        System.err.println( "=== Read Environment Variable and Set to System Property Map ===" );
        Map<String, String> getenv = System.getenv();
        for ( Map.Entry<String,String> e : getenv.entrySet() ) {
            String envKey = e.getKey();
            if ( envKey.toUpperCase().startsWith( "LAMU_" ) ) {
                String propKey = "lamu." + envKey.substring(5).toLowerCase().replaceAll( "_", "-" );
                System.err.println( String.format("%s->%s",envKey, propKey ) );
                System.setProperty( propKey,  e.getValue() );
            }
        }
        System.err.println( "================================================================" );
    }

    static String getSystemProperty( String key ) {
        System.err.println( "Checking SytemProperty :" + key );
        return System.getProperty( key );
    }
    static String setSystemProperty( String key, String value ) {
        System.err.println( "Setting SytemProperty :" + key + "->" + value );
        return System.setProperty( key, value );
    }
    
    /*
     * -Djava.util.logging.config.file=${workspace_loc:lamu}/logging.properties
     */
    static void initLogger() {
        {
            String s = System.getProperty( "lamu.enable-lamu-formatter" );
            if ( s!=null && ! "".equals(s) ) {
                lamu.lib.log.LogFormatter.init();
            }
        }
        {
            String s = System.getProperty( "lamu.logging-properties" );
            if ( s!=null && ! "".equals(s) ) {
                setSystemProperty( "java.util.logging.config.file", s );
            }
        }
        System.err.println( "================================================================" );
    }

    public static void main(String[] args) throws IOException {
        // javax.swing.UIManager.getLookAndFeelDefaults()
        // .put("defaultFont", new java.awt.Font("Data Senenty LET", java.awt.Font.BOLD,
        // 14));

        // MetalLookAndFeel.setCurrentTheme( new LamuMetalTheme() );
        // try {
        // UIManager.setLookAndFeel("javax.swing.plaf.metal.MetalLookAndFeel");
        // } catch (ClassNotFoundException e1) {
        // // TODO Auto-generated catch block
        // e1.printStackTrace();
        // } catch (InstantiationException e1) {
        // // TODO Auto-generated catch block
        // e1.printStackTrace();
        // } catch (IllegalAccessException e1) {
        // // TODO Auto-generated catch block
        // e1.printStackTrace();
        // } catch (UnsupportedLookAndFeelException e1) {
        // // TODO Auto-generated catch block
        // e1.printStackTrace();
        // }


        // Initialize Kawa import path in the first place.
        initKawaImportPath();

        // This causes invoking various initialization procedures.
        loadBasicClasses();

        System.err.println("*** WELCOME TO PULSAR ***");
        System.err.println("VERSION : " + Version.get( Pulsar.class ));
        //		LogFormatter.init();
        LamuPrinter.init();

        //		Logger.getGlobal().setLevel( Level.ALL );

        List<ArgsCommand> availableCommands = createAvailableCommandList();
        ArgsCommandState state = new ArgsCommandState( availableCommands );
        LamuScript.parse( state, args );
        

        List<ApplicationVessel> vesselList = new ArrayList<>( state.vessels );
        Collections.reverse( vesselList );

        
        ApplicationVessel lamu = new ApplicationVessel("lamu-main");
        lamu.addAll( vesselList );
        lamu.add( new InstanceManagerComponent());
        lamu.requestInit();

//        logInfo( "initialize:======= Application:requestInit ==============================" );
//        for ( ApplicationVessel vessel : vesselList ) {
//            if ( false ) {
//                checkRepl( vessel );
//            }
//            logInfo( "initialize:" + vessel );
//            vessel.requestInit();
//        }
//        logInfo( "initialize:==============================================================" );
    }
}
