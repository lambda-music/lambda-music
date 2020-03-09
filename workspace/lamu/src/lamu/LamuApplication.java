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
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import kawapad.Kawapad;
import kawapad.KawapadDocuments;
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
		abstract void    execute(List<LamuApplicationCommand> availableCommands, List<ApplicationComponent> vessels, List<String> arguments, boolean recursiveCall);

		public static void parseSubargs( 
				List<LamuApplicationCommand> availableCommands, 
				List<ApplicationComponent> vessels, List<String> args, boolean recursiveCall )  
		{
			boolean done = false;
			for (LamuApplicationCommand c : availableCommands ) {
				if ( c.match( args ) ) {
					c.execute( availableCommands, vessels, args, recursiveCall );
					done = true;
					break;
				}
			}

			if (!done) {
				// This should not happen because default command always matches.
				throw new Error("unknown command");
			}
		}

		public static List<ApplicationComponent> parseArgs(
				List<LamuApplicationCommand> availableCommands, 
				String[] args ) throws IOException
		{
			List<List<String>> arrayOfSubargs = 
					LamuApplicationArraySplitter.splitBeginEnd(Arrays.asList(args), "begin",  "end");

			List<ApplicationComponent> vessels = new ArrayList<>();
			for (Iterator<List<String>> i = arrayOfSubargs.iterator(); i.hasNext();) {
				List<String> subargs = i.next();
				parseSubargs(availableCommands, vessels, subargs, false);
			}
			return vessels;
		}
	}

	static class LamuApplicationForkCommand extends LamuApplicationCommand {
		static JavaProcess forkPulsar(List<String> arguments) {
			JavaProcess process = new JavaProcess(LamuApplication.class.getCanonicalName(), arguments);
			return process;
		}

		@Override
		boolean match(List<String> arguments) {
			return !arguments.isEmpty() && arguments.get(0).equals("fork");
		}

		@Override
		void execute(List<LamuApplicationCommand> availableCommands, List<ApplicationComponent> vessels, List<String> arguments, boolean recursiveCall) {
			List<String> subArguments = arguments.subList(1, arguments.size());
			// fork
			vessels.add( forkPulsar(subArguments) );
		}
	}

	static class LamuApplicationExecCommand extends LamuApplicationCommand {
		@Override
		boolean match(List<String> arguments) {
			return !arguments.isEmpty() && arguments.get(0).equals("exec");
		}

		@Override
		void execute(List<LamuApplicationCommand> availableCommands, List<ApplicationComponent> vessels, List<String> arguments, boolean recursiveCall) {
			List<String> subArguments = arguments.subList(1, arguments.size());
			// exec
			LamuApplicationArgumentParser argumentParser = new LamuApplicationArgumentParser();
			argumentParser.parse(subArguments);
			vessels.addAll(argumentParser.getApplicationVesselList());
		}
	}

	static class LamuApplicationMacroCommand extends LamuApplicationCommand {
		static final String TAG_END = "__RECALPER__";
		static final String TAG_BEGIN = "__REPLACER__";
		static final Pattern PAT_IN1 = Pattern.compile( "\"([^\"]*)\"" );
		static final Pattern PAT_IN2 = Pattern.compile( "(\\{[^\\{]*?\\})" );
		static final Pattern PAT_OUT = Pattern.compile( TAG_BEGIN + "([0-9]+)" + TAG_END );
		private static String createTag( int number ) {
			return TAG_BEGIN + String.format( "%05x", number  ) + TAG_END;
		}

		static List<String> splitString(String value) {
			List<String> substitution = new ArrayList<>();

			String substitutedValue = value;

			// Substitute every string which is surrounded by a pair of curly brackets.
			// Note that brackets are processed at first.
			{
				boolean found = true;
				while ( found ) {
					Matcher m = PAT_IN2.matcher( substitutedValue );
					StringBuffer sb = new StringBuffer();
					found = false;
					while (m.find()) {
						int idx = substitution.size();  
						substitution.add( m.group(1) );
						m.appendReplacement(sb,  createTag(idx) );
						found = true;
					}
					m.appendTail(sb);
					substitutedValue = sb.toString();
				}
			}

			// Substitute quotated strings.
			// Note that quotations are processed at second.
			{
				Matcher m = PAT_IN1.matcher( substitutedValue );
				StringBuffer sb = new StringBuffer();
				while (m.find()) {
					int idx = substitution.size();  
					substitution.add( m.group(1) );
					m.appendReplacement(sb,  createTag(idx) );
				}
				m.appendTail(sb);
				substitutedValue = sb.toString();
			}

			// The order 1. brackets 2. quotations  MATTERS!
			// This enables usage of pairs of quotations inside a set of brackets.
			
			
//			System.out.println( substitution ); 
			
			List<String> resultList;
			{
				String[] split = substitutedValue.trim().split("[\\s]+");
				for ( int i=0; i<split.length; i++ ) {
					String stagedValue = split[i];
					
					boolean found = true;
					while (found) {
						Matcher m = PAT_OUT.matcher( stagedValue );
						StringBuffer sb = new StringBuffer();
						found = false;
						while (m.find()) {
							found = true;
							int idx = Integer.valueOf( m.group(1), 16);
							String replacement = substitution.get(idx);
							m.appendReplacement( sb,  replacement );
						}
						m.appendTail(sb);
						stagedValue = sb.toString();
					}
					split[i] = stagedValue;
				}
				resultList = Arrays.asList( split );
			}
			return resultList;
		}
		static class Test1 {
			public static String stringify( List<String> lst ) {
				StringBuffer sb = new StringBuffer();
				for ( int i=0; i<lst.size(); i++ ) {
					sb
					.append( '[' )
					.append(i)
					.append( ']')
					.append( " \"" )
					.append( lst.get(i) )
					.append( '"' )
					.append( "\n" );
				}
				return sb.toString();
			}
			static int counter =0;
			public static void output( List<String> lst ) {
				System.out.println( "=== Test" + (counter++) + " ===" );
				System.out.println( stringify( lst ) );
			}
			public static void main(String[] args) {
				
				output( splitString( "hello foo \"bar bum\" world" ) );
				output( splitString( "hello foo \"bar bum\" \"\" world" ) );
				output( splitString( "hello foo \"bar bum\" \"FOO BAR BUM\" world" ) );
				output( splitString( "\"FOO BAR BUM\"" ) );
				output( splitString( "\"FOO BAR BUM\" \"" ) );

				/*
				 *  This method does not throw an error when it encounters to an unterminated quotation.
				 *  This lacks perfection, but it works enough to do the job. Leave it untouched. 
				 *  (Mon, 09 Mar 2020 16:09:06 +0900)  
				 */
				output( splitString( "\"FOO BAR BUM\" \" sss" ) );

				
				output( splitString( "aaa {hello world} bbb" ) );
				output( splitString( "aaa $VAR{hello world} bbb" ) );
				
				/*
				 * This function supports nested curly brackets.
				 * 
				 * [{ ss}, {hello __REPLACER__00000__RECALPER__ world}]
     			 * === Test8 ===
				 * [0] "aaa"
				 * [1] "$VAR{hello { ss} world}"
				 * [2] "bbb"
				 */
				output( splitString( "aaa $VAR{hello { ss} world} bbb" ) );
				output( splitString( "aaa $VAR{hello { FOO BAR } world } bbb" ) );
				
				
				/*
				 * This function supports quotations inside a curly brackets.
				 * 
				 * [{ ss}, {hello __REPLACER__00000__RECALPER__ world}]
     			 * === Test8 ===
				 * [0] "aaa"
				 * [1] "$VAR{hello " FOO BAR " world }"
				 * [2] "bbb"
				 */
				output( splitString( "aaa $VAR{hello \" FOO BAR \" world } bbb" ) );
			}
		}


		static LamuApplicationMacroCommand create(String value) {
			ArrayList<String> list = new ArrayList<>( splitString(value) );
			if (list.size() == 1 && list.get(0).trim().equals("")) {
				return null;
			}
			String macroName = list.remove(0);
			List<String> macroContent = list;
			logInfo( String.format( "macro-from-config [%s]=>%s" , macroName, macroContent.toString() ) );
			return new LamuApplicationMacroCommand(macroName, macroContent);
		}

		static List<LamuApplicationMacroCommand> load(Reader in) throws IOException {
			List<LamuApplicationMacroCommand> result = new ArrayList<>();
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

		static List<LamuApplicationMacroCommand> load( File file ) throws IOException {
			if ( file.exists() && file.isFile() ) {
				try (FileReader f = new FileReader( file )) {
					return load(f);
				}
			} else {
				return Collections.emptyList();
			}
		}

		String macroName;
		List<String> macroContent;

		public LamuApplicationMacroCommand(String macroName, List<String> macroContent) {
			super();
			this.macroName = macroName;
			this.macroContent = macroContent;
		}
		public String getMacroName() {
			return macroName;
		}
		public List<String> getMacroContent() {
			return macroContent;
		}
		@Override
		boolean match(List<String> arguments) {
			return 0 <  arguments.size() && arguments.get(0).equals( this.getMacroName() );
		}

		@Override
		void execute(List<LamuApplicationCommand> availableCommands, List<ApplicationComponent> vessels, List<String> arguments, boolean recursiveCall) {
			if (recursiveCall) {
				throw new Error( "a malformed default value in the default argument configuration." );
			}

			ArrayList<String> outArgs = new ArrayList<>();
			HashMap<String, LamuApplicationNamedArgument> outNargs = new HashMap<>();
			parseArgs(arguments, outArgs, outNargs);
			List<String> expandedArgs = execute(this.macroContent, outArgs, outNargs);
			logInfo( String.format( 
					"MacroCommand[%s] expanded the specified arguments\nfrom:%s\nto  :%s", 
					getMacroName(),
					arguments.toString(),
					expandedArgs.toString() ) );
			LamuApplicationCommand.parseSubargs( availableCommands, vessels, expandedArgs, recursiveCall );
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

					List<String> substList = splitString(subst);

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
					} else if (token.equals("@")) {
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

	static File getInitFile() {
		return new File( System.getProperty("user.home"), ".lamu/default-arguments.conf");
	}
	static List<LamuApplicationCommand> createAvailableCommandList() throws IOException {
		List<LamuApplicationCommand> availableCommands = new ArrayList<>();
		availableCommands.add( new LamuApplicationForkCommand() );
		availableCommands.add( new LamuApplicationExecCommand() );
		availableCommands.addAll( LamuApplicationMacroCommand.load( getInitFile() ) );
		// this is a fall back.
		availableCommands.add( LamuApplicationMacroCommand.create( "default exec scheme + pulsar + gui $*{--open @} +") );
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

		List<LamuApplicationCommand> availableCommands = createAvailableCommandList();
		List<ApplicationComponent> components = LamuApplicationCommand.parseArgs( availableCommands, args );

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
