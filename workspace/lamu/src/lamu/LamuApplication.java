package lamu;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.regex.Pattern;

import kawapad.Kawapad;
import kawapad.KawapadDocuments;
import lamu.LamuApplicationDefaultArgument.Element;
import pulsar.Pulsar;
import pulsar.PulsarDocuments;
import pulsar.lib.swing.PulsarGuiUtils;
import quartz.lib.LogFormatter;
import quartz.lib.Version;
import quartz.lib.app.ApplicationComponent;
import quartz.lib.app.ApplicationVessel;
import quartz.lib.app.process.JavaProcess;
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

	static abstract class LamuApplicationCommand {
		abstract boolean match(List<String> arguments);

		abstract void execute(List<ApplicationComponent> vessels, List<String> arguments, boolean recursiveCall);
	}

	static class LamuApplicationForkCommand extends LamuApplicationCommand {
		@Override
		boolean match(List<String> arguments) {
			return !arguments.isEmpty() && arguments.get(0).equals("fork");
		}

		@Override
		void execute(List<ApplicationComponent> vessels, List<String> arguments, boolean recursiveCall) {
			List<String> subArguments = arguments.subList(1, arguments.size());
			// fork
			vessels.add(forkPulsar(subArguments));
		}
	}

	static class LamuApplicationExecCommand extends LamuApplicationCommand {
		@Override
		boolean match(List<String> arguments) {
			return !arguments.isEmpty() && arguments.get(0).equals("exec");
		}

		@Override
		void execute(List<ApplicationComponent> vessels, List<String> arguments, boolean recursiveCall) {
			List<String> subArguments = arguments.subList(1, arguments.size());
			// exec
			LamuApplicationArgumentParser argumentParser = new LamuApplicationArgumentParser();
			argumentParser.parse(subArguments);
			vessels.addAll(argumentParser.getApplicationVesselList());
		}
	}

	static class LamuApplicationOldDefaultCommand extends LamuApplicationCommand {
		@Override
		boolean match(List<String> arguments) {
			return false;
		}

		@Override
		void execute(List<ApplicationComponent> vessels, List<String> arguments, boolean recursiveCall) {
			if (recursiveCall) {
				throw new Error("a malformed default value in the default argument configuration.");
			}

			try {
				List<Element> defaultArgumentList = LamuApplicationDefaultArgument.load();
				Element defaultArgument = null;
				if (defaultArgumentList.isEmpty()) {
					defaultArgument = new Element("default", "exec scheme + pulsar + pulsar-gui $* +");
				} else {
					defaultArgument = defaultArgumentList.get(0);
				}

				List<String> subArguments = defaultArgument.interpolate(String.join(" ", arguments));

				// a recursive calling
				parseSubargs(vessels, subArguments, true);

			} catch (IOException e) {
				throw new Error(e);
			}
		}

	}

	static class LamuApplicationDefaultCommand extends LamuApplicationCommand {
		public static File getInitFile() {
			return new File(System.getProperty("user.home"), ".kawapad/kawapad-default-arguments.conf");
		}

		static ArrayList<String> splitString(String value) {
			return new ArrayList<>(Arrays.asList(value.trim().split("[\\s]+")));
		}

		static LamuApplicationDefaultCommand create(String value) {
			ArrayList<String> list = splitString(value);
			if (list.size() == 1 && list.get(0).trim().equals("")) {
				return null;
			}
			String macroName = list.remove(0);
			List<String> macroContent = list;
			return new LamuApplicationDefaultCommand(macroName, macroContent);
		}

		static List<LamuApplicationDefaultCommand> load(Reader in) throws IOException {
			List<LamuApplicationDefaultCommand> result = new ArrayList<>();
			try (BufferedReader r = new BufferedReader(in)) {
				for (;;) {
					String s = r.readLine();
					if (s == null)
						break;
					result.add(create(s));
				}
			}
			return result;
		}

		static List<LamuApplicationDefaultCommand> load() throws IOException {
			if (getInitFile().exists() && getInitFile().isFile()) {
			} else {
				return Collections.emptyList();
			}
			try (FileReader f = new FileReader(getInitFile())) {
				return load(f);
			}
		}

		String macroName;
		List<String> macroContent;

		public LamuApplicationDefaultCommand(String macroName, List<String> macroContent) {
			super();
			this.macroName = macroName;
			this.macroContent = macroContent;
		}

		@Override
		boolean match(List<String> arguments) {
			return true;
		}

		@Override
		void execute(List<ApplicationComponent> vessels, List<String> arguments, boolean recursiveCall) {
			if (recursiveCall) {
				throw new Error("a malformed default value in the default argument configuration.");
			}

			ArrayList<String> outArgs = new ArrayList<>();
			HashMap<String, LamuApplicationNamedArgument> outNargs = new HashMap<>();
			parseArgs(arguments, outArgs, outNargs);
			execute(this.macroContent, outArgs, outNargs);
		}

		public static List<String> execute(List<String> macroContent, ArrayList<String> args,
				Map<String, LamuApplicationNamedArgument> namedArgs) {
			ArrayList<String> result = new ArrayList<String>();
			for (Iterator<String> i = macroContent.iterator(); i.hasNext();) {
				String token = i.next().trim();

				if (token.startsWith("$")) {
					// if the current token is a variable; replace the token with the corresponding
					// value.
					token = token.substring(1);

					// the default value as the substitutional string for the variable token.
					String subst = "@";

					// this enables negation of checking existence of the namedArgs.
					boolean expectationForContains = true;
					if (token.startsWith("!")) {
						token = token.substring(1);
						expectationForContains = false;
					}

					//
					int idx0 = token.indexOf("{");
					int idx1 = token.indexOf("}");
					if (0 <= idx0 && 0 <= idx1 && idx0 < idx1) {
						subst = token.substring(idx0 + 1, idx1).trim();
						token = token.substring(0, idx0).trim();
					}

					ArrayList<String> substList = splitString(subst);

					boolean contains = namedArgs.containsKey(token);
					if (expectationForContains == contains) {
						for (Iterator<String> j = substList.iterator(); j.hasNext();) {
							String substToken = j.next();
							if (substToken.equals("@")) {
								result.add(namedArgs.get(token).getValue());
							} else {
								result.add(substToken);
							}
						}
					} else if (Pattern.compile("[0-9]+").matcher(token).matches()) {
						int idx = Integer.valueOf(token);
						if (expectationForContains == (0 <= idx && idx < result.size())) {
							String value = args.get(idx);

							for (Iterator<String> j = substList.iterator(); j.hasNext();) {
								String substToken = j.next();
								if (substToken.equals("@")) {
									result.add(value);
								} else {
									result.add(substToken);
								}
							}
						}
					} else if (token.equals("*")) {
						if (expectationForContains == (!result.isEmpty())) {
							for (Iterator<String> j = substList.iterator(); j.hasNext();) {
								String substToken = j.next();
								if (substToken.equals("@")) {
									result.addAll(args);
								} else {
									result.add(substToken);
								}
							}
						}
					}
				} else {
					// Otherwise, simply add the current token.
					result.add(token);
				}
			}
			return result;
		}

		static void parseArgs(List<String> in, List<String> args, HashMap<String, LamuApplicationNamedArgument> nargs) {
			for (Iterator<String> i = in.iterator(); i.hasNext();) {
				String token = i.next();
				if (token.startsWith("--")) {
					LamuApplicationNamedArgument na = new LamuApplicationNamedArgument(token);
					nargs.put(na.getKey(), na);
				} else {
					args.add(token);
				}
			}
		}

	}

	static final List<LamuApplicationCommand> commandList = new ArrayList<>();
	static {
		commandList.add(new LamuApplicationForkCommand());
		commandList.add(new LamuApplicationExecCommand());
		commandList.add(new LamuApplicationOldDefaultCommand());
		try {
			List<LamuApplicationDefaultCommand> list = LamuApplicationDefaultCommand.load();
			if (list.isEmpty()) {
				commandList.add(LamuApplicationDefaultCommand.create( "exec scheme + pulsar + gui $*{--open @} +") );
			} else {
				commandList.add(list.get(0));
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	private static void parseSubargs(List<ApplicationComponent> vessels, List<String> args, boolean recursiveCall)
			throws IOException {
		boolean done = false;
		for (LamuApplicationCommand c : commandList) {
			if (c.match(args)) {
				c.execute(vessels, args, recursiveCall);
				done = true;
				break;
			}
		}

		if (!done) {
			// This should not happen because default command always matches.
			throw new Error("unknown command");
		}
	}

	static List<ApplicationComponent> parseArgs(String[] args) throws IOException {
		List<List<String>> arrayOfSubargs = LamuApplicationArraySplitter.splitBeginEnd(Arrays.asList(args), "begin",
				"end");

		List<ApplicationComponent> vessels = new ArrayList<>();
		for (Iterator<List<String>> i = arrayOfSubargs.iterator(); i.hasNext();) {
			List<String> subargs = i.next();
			parseSubargs(vessels, subargs, false);
		}
		return vessels;
	}

	static JavaProcess forkPulsar(List<String> arguments) {
		JavaProcess process = new JavaProcess(LamuApplication.class.getCanonicalName(), arguments);
		return process;
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

		List<ApplicationComponent> components = parseArgs(args);

		ApplicationVessel owner = new ApplicationVessel();
		owner.addAll(components);
		owner.requestInit();

		Thread thread = new Thread(new Runnable() {
			@Override
			public void run() {
				BufferedReader r = new BufferedReader(new InputStreamReader(System.in));
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
