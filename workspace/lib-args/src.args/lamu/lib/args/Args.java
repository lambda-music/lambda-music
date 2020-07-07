package lamu.lib.args;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.regex.Pattern;

import lamu.lib.logging.Logger;

public class Args {
    static final Logger LOGGER = Logger.getLogger(MethodHandles.lookup().lookupClass().getName());
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg) { LOGGER.log(Level.INFO, msg); }
    static void logWarn(String msg) { LOGGER.log(Level.WARNING, msg); }

    static final int RECURSIVE_COUNT_MAX = 32;
    public static final String VARIABLE_MARK = "$";
    public static final String DEFAULT_BRACE_BLOCK_SCRIPT = VARIABLE_MARK;
    public static final String SHEBANG = "#!";

    public static <State extends ArgsCommandState> void executeScript( State state, List<String> arguments ) {
        executeScript( state, arguments, 0 );
    }
    
    public static <State extends ArgsCommandState> void executeScript( State state, List<String> arguments, int recursiveCount ) {
        List<List<String>> arrayOfSubarguments = 
                ArgsBeginEndSplitter.splitBeginEnd( arguments, "begin",  "end" );

        for (Iterator<List<String>> i = arrayOfSubarguments.iterator(); i.hasNext();) {
            List<String> subargs = i.next();
            logInfo( subargs.toString() );
            executeSubScript( state, subargs, recursiveCount );
        }
    }
    
    public static void executeSubScript( ArgsCommandState state, List<String> arguments, int recursiveCount ) {
        ArgsCommand command = null;
        for (ArgsCommand c : state.availableCommands ) {
            if ( c.match( state, arguments ) ) {
                command = c;
                break;
            }
        }

        if ( command != null ) {
            List<String> subArguments = arguments.subList(1, arguments.size());
            command.execute( state, subArguments, recursiveCount );
        } else {
            // This should not happen because default command always matches.
            throw new Error( String.format( "unknown command (%s) %s" , 
                arguments.isEmpty() ? "~empty~" : arguments.get(0) , arguments.toString() ));
        }
    }
    

    public static void executeMacro( 
        ArgsCommandState state, 
        String scriptName, 
        List<String> scriptContent,
        List<String> originalArguments,
        List<String> seqArgs,
        Map<String,ArgsNamedArgument> namedArgs, 
        int recursiveCount )
    {

        // perform macro expansion.
        List<String> expandedArgs = expandMacro( scriptContent, seqArgs, namedArgs );

        logInfo( String.format( 
                "\nLamuScript[%s] expanded the specified arguments\nrecursive count:%d\nfrom:%s\nto  :%s\nmacro:%s\nargs:%s\nnargs:%s\n", 
                scriptName,
                recursiveCount,
                originalArguments.toString(),
                expandedArgs.toString(),
                scriptContent.toString(),
                seqArgs.toString(),
                namedArgs.toString()
            ) );

        // Be careful : this is a recursive calling. 
        executeScript( state, expandedArgs, recursiveCount + 1 );
    }
    
    public static void parseArguments( List<String> arguments, List<String> outSeqArgs, Map<String, ArgsNamedArgument> outNamedArgs) {
        for (Iterator<String> i = arguments.iterator(); i.hasNext();) {
            String token = i.next();
            if (token.startsWith("--")) {
                ArgsNamedArgument na = new ArgsNamedArgument(token);
                outNamedArgs.put(na.getKey(), na);
            } else if (token.startsWith("-")) {
                ArgsNamedArgument na = new ArgsNamedArgument( token.substring(1,2), token.substring(2) );
                outNamedArgs.put(na.getKey(), na);
            } else {
                outSeqArgs.add(token);
            }
        }
    }
    
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    private static void replaceProc( List<String> result, String tokenToAdd, String replaceFrom, List<String> replaceTo ) {
        if ( tokenToAdd.equals( replaceFrom )) {
            result.addAll( replaceTo );
        } else {
            String modifiedTokenToAdd = 
                Pattern
                    .compile( Pattern.quote( replaceFrom ) )
                    .matcher( tokenToAdd).replaceAll(
                        String.join( " ", replaceTo ));
            if ( tokenToAdd.equals( modifiedTokenToAdd ) ) {
                result.add(
                    tokenToAdd.replaceAll( 
                        Pattern.quote( replaceFrom ), 
                        String.join( " ", replaceTo )));
            } else {
                result.add( modifiedTokenToAdd );
            }
        }
    }

    public static List<String> expandMacro( 
        List<String> macroContent, 
        List<String> args, 
        Map<String, ArgsNamedArgument> namedArgs) 
    {
        ArrayList<String> result = new ArrayList<String>();
        for (Iterator<String> i = macroContent.iterator(); i.hasNext();) {
            String token = i.next().trim();

            if ( token.startsWith(SHEBANG)) {
                // If the current token is SHEBANG line, ignore the line. 
            } else if (token.startsWith(VARIABLE_MARK)) {
                // if the current token is a variable; replace the token with the corresponding
                // value.
                token = token.substring(1);

                // the default value as the substitutional string for the variable token.
                String subst = DEFAULT_BRACE_BLOCK_SCRIPT;

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

                List<String> substList = ArgsQuotedStringSplitter.splitString(subst);

                boolean contains = namedArgs.containsKey(token);
                if (expectationForContains == contains) {
                    for (Iterator<String> j = substList.iterator(); j.hasNext();) {
                        String substToken = j.next();
                        String replaceTo = namedArgs.get(token).getValue();
                        replaceProc(result, substToken, VARIABLE_MARK, Arrays.asList( replaceTo ));
                    }
                } else if (Pattern.compile("[0-9]+").matcher(token).matches()) {
                    int idx = Integer.valueOf(token);
                    if (expectationForContains == (0 <= idx && idx < result.size())) {
                        String replaceTo = args.get(idx);
                        for (Iterator<String> j = substList.iterator(); j.hasNext();) {
                            String substToken = j.next();
                            replaceProc(result, substToken, VARIABLE_MARK, Arrays.asList( replaceTo ));
                        }
                    }
                } else if (token.equals("*")) {
                    if (expectationForContains == (! args.isEmpty())) {
                        for (Iterator<String> j = substList.iterator(); j.hasNext();) {
                            String substToken = j.next();
                            List<String> replaceTo = args;
                            replaceProc(result, substToken, VARIABLE_MARK, replaceTo );
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
}
