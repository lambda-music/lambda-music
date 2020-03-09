package lamu;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;

import kawapad.Kawapad;
import kawapad.KawapadDocuments;
import pulsar.Pulsar;
import pulsar.PulsarDocuments;
import pulsar.lib.swing.PulsarGuiUtils;
import quartz.lib.LogFormatter;
import quartz.lib.Version;
import quartz.lib.app.ApplicationComponent;
import quartz.lib.app.ApplicationVessel;
import quartz.lib.log.SimpleConsoleLogger;
import quartz.lib.scheme.doc.DescriptiveHelp;

public class LamuApplication {
	static final SimpleConsoleLogger LOGGER = SimpleConsoleLogger
			.getLogger(MethodHandles.lookup().lookupClass().getName());

	static void logError(String msg, Throwable e) {
		LOGGER.log(Level.SEVERE, msg, e);
	}

	static void logInfo(String msg) {
		LOGGER.log(Level.INFO, msg);
	}

	static void logWarn(String msg) {
		LOGGER.log(Level.WARNING, msg);
	}

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
				LamuCommand.DEFAULT_COMMAND_NAME + " exec scheme + pulsar + gui $*{--open @} +") );
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
		LogFormatter.init();
		LamuPrinter.init();

		List<LamuCommand> availableCommands = createAvailableCommandList();
		List<ApplicationComponent> components = LamuCommand.parseArgs( availableCommands, args );

		ApplicationVessel owner = new ApplicationVessel();
		owner.addAll(components);
		owner.requestInit();

		Thread thread = new Thread( new Runnable() {
			@Override
			public void run() {
				BufferedReader r = new BufferedReader( new InputStreamReader(System.in) );
				try {
					try {
						for (;;) {
							String s = r.readLine();
							if (s == null)
								break;
							if ("quit".equals(s) || "bye".equals(s)) {
								System.out.println("ok");
								owner.processQuit();
								break;
							} else if ("alive?".equals(s)) {
								System.out.println("yes");
							} else if ("hello".equals(s)) {
								System.out.println("hello");
							} else {
								System.out.println("unknown-command");
							}
						}
					} finally {
						r.close();
					}
				} catch (IOException e) {
					logError("", e);
				}
			}
		}, "command-reception");
		thread.setDaemon(true);
		thread.start();
	}
}
