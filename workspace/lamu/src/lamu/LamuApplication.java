package lamu;

import java.io.File;
import java.io.IOException;
import java.lang.invoke.MethodHandles;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Deque;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Level;

import kawapad.Kawapad;
import kawapad.KawapadDocuments;
import lamu.lib.Version;
import lamu.lib.app.ApplicationComponent;
import lamu.lib.app.ApplicationVessel;
import lamu.lib.log.Logger;
import lamu.lib.scheme.doc.DescriptiveHelp;
import lamu.lib.scheme.repl.SimpleReplService;
import lamu.lib.stream.SisoReceiver;
import lamu.utils.lib.PulsarGuiUtils;
import pulsar.Pulsar;
import pulsar.PulsarDocuments;

public class LamuApplication {
    static final Logger LOGGER = Logger.getLogger(MethodHandles.lookup().lookupClass().getName());
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg) { LOGGER.log(Level.INFO, msg); }
    static void logWarn(String msg) { LOGGER.log(Level.WARNING, msg); }
    private LamuApplication() {
    }

    static File getInitFile() {
        return new File( System.getProperty("user.home"), ".lamu/default-arguments.conf");
    }
    static List<LamuCommand> createAvailableCommandList() throws IOException {
        List<LamuCommand> availableCommands = new ArrayList<>();
        availableCommands.add( new LamuCommandFork() );
        availableCommands.add( new LamuCommandExec() );
        availableCommands.addAll( LamuCommandMacro.load( getInitFile() ) );
        // this is a fall back.
        availableCommands.add( LamuCommandMacro.create( 
                LamuCommand.DEFAULT_COMMAND_NAME + " exec scheme + pulsar + repl + gui $*{$} +") );
        return availableCommands;
    }

    private static void forceLoad(Class c) {
        try {
            Class.forName(c.getName(), true, c.getClassLoader());
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        }
    }

    static void loadBasicClasses() {
        // For documentation.
        forceLoad(PulsarDocuments.class);
        forceLoad(PulsarGuiUtils.class);
        forceLoad(KawapadDocuments.class);
        forceLoad(DescriptiveHelp.class);

        // See those static blocks.
        forceLoad(Kawapad.class);
        forceLoad(Pulsar.class);
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
        System.err.println("VERSION : " + Version.get(Pulsar.class));
        //		LogFormatter.init();
        LamuPrinter.init();

        //		Logger.getGlobal().setLevel( Level.ALL );

        Collection<LamuCommand> availableCommands = createAvailableCommandList();
        Deque<ApplicationVessel> vessels = new ArrayDeque<>();
        LamuCommand.parseArgs( availableCommands, vessels, args );

        List<ApplicationVessel> vesselList=  new ArrayList<>( vessels );
        Collections.reverse( vesselList );

        for ( ApplicationVessel vessel : vesselList ) {
            if ( false ) {
                checkRepl( vessel );
            }
            vessel.requestInit();
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
            //				Thread thread = new Thread( new LamuSimpleSocketServer( owner, System.in, System.out), "command-reception" );
            //				thread.setDaemon(true);
            //				thread.start();
            vessel.add( new SisoReceiver( null, System.in, System.out, new SimpleReplService() ) );
        }
    }
}
