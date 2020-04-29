package lamu.main;

import java.io.File;
import java.io.IOException;
import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;

import kawapad.Kawapad;
import kawapad.KawapadTextualIncrement;
import lamu.lib.ConsoleChecker;
import lamu.lib.ForceLoadingClass;
import lamu.lib.Version;
import lamu.lib.apps.ApplicationComponent;
import lamu.lib.apps.ApplicationVessel;
import lamu.lib.args.Args;
import lamu.lib.args.ArgsCommand;
import lamu.lib.args.ArgsCommandEcho;
import lamu.lib.args.ArgsCommandExec;
import lamu.lib.args.ArgsCommandFork;
import lamu.lib.args.ArgsCommandLoad;
import lamu.lib.args.ArgsCommandMacro;
import lamu.lib.args.ArgsCommandState;
import lamu.lib.args.ArgsNamedArgument;
import lamu.lib.args.forking.ForkedProcess;
import lamu.lib.evaluators.EvaluatorLib;
import lamu.lib.evaluators.repl.SimpleReplService;
import lamu.lib.helps.LamuDocument;
import lamu.lib.log.LogFormatter;
import lamu.lib.log.Logger;
import lamu.lib.procs.InstanceManagerComponent;
import lamu.lib.streams.NullStream;
import lamu.lib.streams.SisoReceiver;
import lamu.lib.streams.StdioStream;
import lamu.utils.lib.PulsarGuiUtils;
import pulsar.Pulsar;
import pulsar.PulsarLib;
import pulsar.PulsarLib_Notes;
import pulsar.PulsarLib_Procs;

public class LamuApplication {
    static final Logger LOGGER = Logger.getLogger(MethodHandles.lookup().lookupClass().getName());
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg) { LOGGER.log(Level.INFO, msg); }
    static void logWarn(String msg) { LOGGER.log(Level.WARNING, msg); }
    
    //
    private LamuApplication() {
    }

    // (Sun, 29 Mar 2020 23:16:11 +0900)
    static {
        readEnvironmentToSystemPropertyMap();
        initLogger();
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
        ForceLoadingClass.force(lamu.procs.class );

        ForceLoadingClass.force(EvaluatorLib.class);

        // See those static blocks.
        ForceLoadingClass.force(Kawapad.class);
        ForceLoadingClass.force(KawapadTextualIncrement.class);
        ForceLoadingClass.force(Pulsar.class);
        ForceLoadingClass.force(PulsarLib_Procs.class);
        
        LamuDocument.debugOut();
    }
    static File getInitFile() {
        return new File( System.getProperty("user.home"), ".lamu/default-arguments.conf");
    }
    
    static final String TRIGGER_FOR_ADVANCED_COMMAND_MODE = "advanced";
    static final String DEFAULT_COMMAND_OPEN = "open";
    static final String DEFAULT_COMMAND_EXEC = "exec";
    static final String DEFAULT_COMMAND_LOAD = "load";
    static final String DEFAULT_COMMAND      = "default";

    
    static List<ArgsCommand> createAvailableCommandList() throws IOException {
        List<ArgsCommand> availableCommands = new ArrayList<>();
        availableCommands.add( new LamuApplicationBuilder( "create" ) );
        
        ArgsCommandFork.ForkListener forkListener = new ArgsCommandFork.ForkListener() {
            @Override
            public void notifyForkedProcess( ArgsCommandState state0, ForkedProcess process ) {
                LamuCommandState state = (LamuCommandState) state0;
                
                // Create a vessel and put it to the vessel list.
                ApplicationVessel vessel = new ApplicationVessel( "ForkedVessel" );
                vessel.add( process );
                
                // Push it to the stack for vessels.
                state.vessels.push( vessel );
            }
        };
        
        availableCommands.add( new ArgsCommandFork( "lamu", LamuApplication.class, forkListener ));
        availableCommands.add( new ArgsCommandLoad( "load" ) );
        availableCommands.add( new ArgsCommandExec( DEFAULT_COMMAND_EXEC ) );
        availableCommands.add( new ArgsCommandEcho( "echo" ) );
        availableCommands.addAll( ArgsCommandMacro.load( getInitFile() ) );

        // this is a fall back.
        availableCommands.add( ArgsCommandMacro.create( 
                    DEFAULT_COMMAND_LOAD + " " + 
                    " create scheme + repl $*{--load=$} +" ));

        availableCommands.add( ArgsCommandMacro.create( 
                    DEFAULT_COMMAND_OPEN + " " + 
                    " create scheme + repl + gui $*{$} +" ));
        
        availableCommands.add( ArgsCommandMacro.create( 
            DEFAULT_COMMAND + " " + 
            DEFAULT_COMMAND_EXEC + " $*{$}" ));

        return availableCommands;
    }

    
    public static void executeScript( ArgsCommandState state, String[] args ) throws IOException {
        executeScript0( state, new ArrayList<>( Arrays.asList(args)) );
    }
    public static void executeScript0(ArgsCommandState state, List<String> arguments) {
        // Put a proper default command.
        defaultCommandInterpolation( arguments ); 

        Args.executeScript( state, arguments );
    }

    /**
     * Perform the default command interpolation on the passed argument list.
     * 
     * If the first element is not TRIGGER_FOR_ADVANCED_COMMAND_MODE, regard it as
     * the "default mode". Let's put the TRIGGER_FOR_ADVANCED_COMMAND_MODE and a
     * propert default command name which fits to the condition.
     * 
     * @param arguments
     *    the target argument list
     */
    static void defaultCommandInterpolation(List<String> arguments) {
        if ( arguments.isEmpty() ) {
            // --open/-o are equivalent to --command=open
            logInfo( "default mode command : no arg; therefore, forwarded to `command=open`" );
            arguments.addAll( 0, Arrays.asList(  TRIGGER_FOR_ADVANCED_COMMAND_MODE, DEFAULT_COMMAND_OPEN ) );
            
        } else if ( ! TRIGGER_FOR_ADVANCED_COMMAND_MODE.equals( arguments.get( 0 ))) {
            List<String> outSeqArgs = new ArrayList<>();
            Map<String, ArgsNamedArgument> outNamedArgs = new HashMap<>();
            Args.parseArguments( arguments, outSeqArgs, outNamedArgs );

            if ( false ) {
                // 
            } else if ( outNamedArgs.containsKey( "command" ) ) {
                // --command=[command-name] invokes a specific command   
                String commandName = outNamedArgs.get( "command" ).getValue();
                logInfo( "default mode command : --command=" + commandName );
                arguments.addAll( 0, Arrays.asList(  TRIGGER_FOR_ADVANCED_COMMAND_MODE, commandName ) );

                
            } else if ( outNamedArgs.containsKey( "exec" ) || outNamedArgs.containsKey( "e" ) ) {
                // --exec/-e are equivalent to --command=exec
                logInfo( "default mode command : --exec" );
                arguments.addAll( 0, Arrays.asList(  TRIGGER_FOR_ADVANCED_COMMAND_MODE, DEFAULT_COMMAND_EXEC ) );
                
            } else if ( outNamedArgs.containsKey( "load" )) {
                // --load is equivalent to --command=load. The purpos of this command is mainly
                // to support shebang. Shebang accepts only one command-line argument with only one filename;
                // shebang cannot accept command-line arguments more than two.
                
                String fileName = outNamedArgs.get( "load" ).getValue();
                logInfo( "default mode command : --load=" + fileName );
                arguments.addAll( 0, Arrays.asList(  TRIGGER_FOR_ADVANCED_COMMAND_MODE, DEFAULT_COMMAND_LOAD, fileName ) );
            } else if ( outNamedArgs.containsKey( "open" ) || outNamedArgs.containsKey( "o" ) ) {
                // --open/-o are equivalent to --command=open
                logInfo( "default mode command : --open" );
                arguments.addAll( 0, Arrays.asList(  TRIGGER_FOR_ADVANCED_COMMAND_MODE, DEFAULT_COMMAND_OPEN ) );
            } else {
                // The default command. This is equivalent to `--command=default`. See  DEFAULT_COMMAND.
                arguments.addAll( 0, Arrays.asList(  TRIGGER_FOR_ADVANCED_COMMAND_MODE, DEFAULT_COMMAND ) );
            }
        }

        // Remove the first element; at this point, it always equals to TRIGGER_FOR_ADVANCED_COMMAND_MODE. 
        if ( ! TRIGGER_FOR_ADVANCED_COMMAND_MODE.equals( arguments.remove(0))) {
            throw new Error( "internal error" );
        };
    }

    
    
    
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    
    
    
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
    

    /**
     * Set the default stream. In case it is executed by javaw (windows) STDIO is
     * not available and writing/reading from it causes a runtime exception to be
     * thrown which is not preferable. In order to avoid the exception, check
     * System.console(). When it returns null, it is likely that the current jvm is
     * executed from javaw.
     * 
     * See {@link lamu.LamuApplicationArgumentParser.StdioArgumentParserElementFactory}.
     * 
     * (Sun, 29 Mar 2020 03:35:24 +0900)
     * 
     * (Tue, 21 Apr 2020 12:27:44 +0900)
     * Moved from {@link lamu.lib.args.ArgsCommandState}
     */
    static void initState( LamuCommandState state ) {
        if ( ConsoleChecker.consoleExists() ) {
            state.streamables.push( StdioStream.INSTANCE );
        } else {
            state.streamables.push( NullStream.INSTANCE );
        }
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

        // Revived (Fri, 24 Apr 2020 15:43:57 +0900)
        LogFormatter.init();

        // Initialize Kawa import path in the first place.
        initKawaImportPath();

        // This causes invoking various initialization procedures.
        loadBasicClasses();

        System.err.println("*** WELCOME TO LAMU APPLICATION ***");
        System.err.println("VERSION : " + Version.get( Pulsar.class ));
        //		LogFormatter.init();
        LamuPrinter.init();

        //		Logger.getGlobal().setLevel( Level.ALL );

        List<ArgsCommand> availableCommands = createAvailableCommandList();
        LamuCommandState state = new LamuCommandState( availableCommands );
        initState( state );
        executeScript( state, args );
        

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
