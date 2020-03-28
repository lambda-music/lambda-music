package lamu;

import java.io.IOException;
import java.lang.invoke.MethodHandles;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Deque;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.regex.Pattern;

import lamu.lib.app.ApplicationVessel;
import lamu.lib.log.Logger;
import lamu.lib.stream.NullStream;
import lamu.lib.stream.StdioStream;
import lamu.lib.stream.Stream;

public class LamuScript {
    public static final class State {
        Collection<LamuCommand> availableCommands; 
        Deque<ApplicationVessel> vessels;
        Deque<Stream> streamables;
        public State( Collection<LamuCommand> availableCommands ) {
            super();
            this.availableCommands = availableCommands;
            this.vessels = new ArrayDeque<>();
            this.streamables = new ArrayDeque<>();
            
            /*
             * Set the default stream. In case it is executed by javaw (windows) STDIO is
             * not available and writing/reading from it causes a runtime exception to be
             * thrown which is not preferable. In order to avoid the exception, check  
             * System.console(). When it returns null, it is likely that the current
             * jvm is executed from javaw.
             * 
             * (Sun, 29 Mar 2020 03:35:24 +0900)
             */
            if ( System.console() == null ) {
                this.streamables.push( StdioStream.INSTANCE );
            } else {
                this.streamables.push( NullStream.INSTANCE );
            }
        }
    }
    
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    static final String TRIGGER_FOR_ADVANCED_COMMAND_MODE = "advanced";
    static final String DEFAULT_COMMAND_NAME = "default";
    
    static final Logger LOGGER = Logger.getLogger(MethodHandles.lookup().lookupClass().getName());
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg) { LOGGER.log(Level.INFO, msg); }
    static void logWarn(String msg) { LOGGER.log(Level.WARNING, msg); }
    
    public static void parse( LamuScript.State state, String[] args ) throws IOException {
        executeScript(state, Arrays.asList(args), false );
    }

    public static void executeScript( LamuScript.State state, List<String> arguments, boolean isRecursiveCall  ) {
        arguments = new ArrayList<>( arguments );

        // (Mon, 09 Mar 2020 23:39:18 +0900) 
        // If the first element is not TRIGGER_FOR_ADVANCED_COMMAND_MODE,
        // regard it as the "default mode". Let's put the TRIGGER_FOR_ADVANCED_COMMAND_MODE and
        // the default command name. 
        if ( arguments.size() == 0 || ! TRIGGER_FOR_ADVANCED_COMMAND_MODE.equals( arguments.get( 0 ))) {
            arguments.addAll( 0, Arrays.asList(  TRIGGER_FOR_ADVANCED_COMMAND_MODE, DEFAULT_COMMAND_NAME ) );
        } 

        // Remove the first element; at this point, it always equals to TRIGGER_FOR_ADVANCED_COMMAND_MODE. 
        arguments.remove(0);

        List<List<String>> arrayOfSubarguments = 
                LamuBeginEndSplitter.splitBeginEnd( arguments, "begin",  "end" );

        for (Iterator<List<String>> i = arrayOfSubarguments.iterator(); i.hasNext();) {
            List<String> subargs = i.next();
            logInfo( subargs.toString() );
            executeSubScript( state, subargs, isRecursiveCall );
        }
    }
    
    public static void executeSubScript( LamuScript.State state, List<String> arguments, boolean isRecursiveCall ) {
        LamuCommand command = null;
        for (LamuCommand c : state.availableCommands ) {
            if ( c.match( state, arguments ) ) {
                command = c;
                break;
            }
        }

        if ( command != null ) {
            List<String> subArguments = arguments.subList(1, arguments.size());
            command.execute( state, subArguments, isRecursiveCall );
        } else {
            // This should not happen because default command always matches.
            throw new Error( String.format( "unknown command (%s)" , 
                arguments.isEmpty() ? "~empty~" : arguments.get(0) ));
        }
    }
    

    public static void executeMacro( 
        LamuScript.State state, 
        String scriptName, 
        List<String> scriptContent, 
        List<String> arguments )
    {
        ArrayList<String> outArgs = new ArrayList<>();
        HashMap<String, LamuNamedArgument> outNamedArgs = new HashMap<>();

        // parse the passed arguments
        parseMacro( arguments, outArgs, outNamedArgs );

        // perform macro expansion.
        List<String> expandedArgs = expandMacro( scriptContent, outArgs, outNamedArgs );

        LamuApplication.logInfo( String.format( 
                "\nLamuScript[%s] expanded the specified arguments\nfrom:%s\nto  :%s\nmacro:%s\nargs:%s\nnargs:%s\n", 
                scriptName,
                arguments.toString(),
                expandedArgs.toString(),
                scriptContent.toString(),
                outArgs.toString(),
                outNamedArgs.toString()
            ) );

        // Be careful : this is a recursive calling. 
        executeScript( state, expandedArgs, true );
    }
    
    public static void parseMacro(List<String> in, List<String> outArgs, Map<String, LamuNamedArgument> outNamedArgs) {
        for (Iterator<String> i = in.iterator(); i.hasNext();) {
            String token = i.next();
            if (token.startsWith("--")) {
                LamuNamedArgument na = new LamuNamedArgument(token);
                outNamedArgs.put(na.getKey(), na);
            } else {
                outArgs.add(token);
            }
        }
    }

    private static void replaceProc( List<String> result, String tokenToAdd, String replaceFrom, List<String> replaceTo ) {
        if ( tokenToAdd.equals( replaceFrom )) {
            result.addAll( replaceTo );
        } else {
            result.add(
                tokenToAdd.replaceAll( 
                    Pattern.quote( replaceFrom ), 
                    String.join( " ", replaceTo )));
        }
    }

    public static List<String> expandMacro( 
        List<String> macroContent, 
        List<String> args, 
        Map<String, LamuNamedArgument> namedArgs) 
    {
        ArrayList<String> result = new ArrayList<String>();
        for (Iterator<String> i = macroContent.iterator(); i.hasNext();) {
            String token = i.next().trim();

            if (token.startsWith("$")) {
                // if the current token is a variable; replace the token with the corresponding
                // value.
                token = token.substring(1);

                // the default value as the substitutional string for the variable token.
                String subst = "$";

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
                        String replaceTo = namedArgs.get(token).getValue();
                        replaceProc(result, substToken, "$", Arrays.asList( replaceTo ));
                    }
                } else if (Pattern.compile("[0-9]+").matcher(token).matches()) {
                    int idx = Integer.valueOf(token);
                    if (expectationForContains == (0 <= idx && idx < result.size())) {
                        String replaceTo = args.get(idx);
                        for (Iterator<String> j = substList.iterator(); j.hasNext();) {
                            String substToken = j.next();
                            replaceProc(result, substToken, "$", Arrays.asList( replaceTo ));
                        }
                    }
                } else if (token.equals("*")) {
                    if (expectationForContains == (! args.isEmpty())) {
                        for (Iterator<String> j = substList.iterator(); j.hasNext();) {
                            String substToken = j.next();
                            List<String> replaceTo = args;
                            replaceProc(result, substToken, "$", replaceTo );
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
