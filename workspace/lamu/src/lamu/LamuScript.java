package lamu;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

public class LamuScript {
    public static void execute( LamuCommand.State state, String scriptName, List<String> scriptContent, List<String> arguments ) {
        ArrayList<String> outArgs = new ArrayList<>();
        HashMap<String, LamuNamedArgument> outNamedArgs = new HashMap<>();

        // parse the passed arguments
        parseArguments( arguments, outArgs, outNamedArgs );

        // perform macro expansion.
        List<String> expandedArgs = executeMacro( scriptContent, outArgs, outNamedArgs );

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
        LamuCommand.parseSubargs( state, expandedArgs, true );
    }
    
    public static void parseArguments(List<String> in, List<String> outArgs, Map<String, LamuNamedArgument> outNamedArgs) {
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

    public static List<String> executeMacro( 
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
