package lamu;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import quartz.lib.app.ApplicationComponent;

class LamuCommandMacro extends LamuCommand {
	static LamuCommandMacro create(String value) {
		ArrayList<String> list = new ArrayList<>( LamuQuotedStringSplitter.splitString(value) );
		if (list.size() == 1 && list.get(0).trim().equals("")) {
			return null;
		}
		String macroName = list.remove(0);
		List<String> macroContent = list;
		LamuApplication.logInfo( String.format( "macro-from-config [%s]=>%s" , macroName, macroContent.toString() ) );
		return new LamuCommandMacro(macroName, macroContent);
	}

	static List<LamuCommandMacro> load(Reader in) throws IOException {
		List<LamuCommandMacro> result = new ArrayList<>();
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

	static List<LamuCommandMacro> load( File file ) throws IOException {
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

	public LamuCommandMacro(String macroName, List<String> macroContent) {
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
	void execute(List<LamuCommand> availableCommands, List<ApplicationComponent> vessels, List<String> arguments, boolean recursiveCall) {
		if (recursiveCall) {
			throw new Error( "a malformed default value in the default argument configuration." );
		}

		ArrayList<String> outArgs = new ArrayList<>();
		HashMap<String, LamuNamedArgument> outNargs = new HashMap<>();
		parseArgs(arguments, outArgs, outNargs);
		List<String> expandedArgs = execute(this.macroContent, outArgs, outNargs);
		LamuApplication.logInfo( String.format( 
				"MacroCommand[%s] expanded the specified arguments\nfrom:%s\nto  :%s", 
				getMacroName(),
				arguments.toString(),
				expandedArgs.toString() ) );
		LamuCommand.parseSubargs( availableCommands, vessels, expandedArgs, recursiveCall );
	}

	public static List<String> execute(List<String> macroContent, ArrayList<String> args,
			Map<String, LamuNamedArgument> namedArgs) {
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

				List<String> substList = LamuQuotedStringSplitter.splitString(subst);

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

	static void parseArgs(List<String> in, List<String> args, HashMap<String, LamuNamedArgument> nargs) {
		for (Iterator<String> i = in.iterator(); i.hasNext();) {
			String token = i.next();
			if (token.startsWith("--")) {
				LamuNamedArgument na = new LamuNamedArgument(token);
				nargs.put(na.getKey(), na);
			} else {
				args.add(token);
			}
		}
	}

}