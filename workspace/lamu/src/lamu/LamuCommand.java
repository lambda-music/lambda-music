package lamu;

import java.io.IOException;
import java.lang.invoke.MethodHandles;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Deque;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Level;

import lamu.lib.app.ApplicationComponent;
import lamu.lib.log.Logger;

abstract class LamuCommand {
    static final Logger LOGGER = Logger.getLogger(MethodHandles.lookup().lookupClass().getName());
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg) { LOGGER.log(Level.INFO, msg); }
    static void logWarn(String msg) { LOGGER.log(Level.WARNING, msg); }

    abstract boolean match(   List<String> arguments );
    abstract void    execute( Deque<Object> globalValueStack, List<LamuCommand> availableCommands, List<ApplicationComponent> vessels, List<String> arguments, boolean recursiveCall);

    public static void parseSubargs( 
            Deque<Object> globalValueStack, 
            List<LamuCommand> availableCommands, List<ApplicationComponent> vessels, List<String> args, boolean isRecursiveCall )  
    {
        boolean done = false;
        for (LamuCommand c : availableCommands ) {
            if ( c.match( args ) ) {
                c.execute( globalValueStack, availableCommands, vessels, args, isRecursiveCall );
                done = true;
                break;
            }
        }

        if (!done) {
            // This should not happen because default command always matches.
            throw new Error("unknown command");
        }
    }

    static final String TRIGGER_FOR_ADVANCED_COMMAND_MODE = "do";
    static String DEFAULT_COMMAND_NAME = "default";

    public static List<ApplicationComponent> parseArgs(
            List<LamuCommand> availableCommands, 
            String[] in_args ) throws IOException
    {
        // (Mon, 09 Mar 2020 23:39:18 +0900) 
        // If the first element is not TRIGGER_FOR_ADVANCED_COMMAND_MODE,
        // regard it as the "default mode". Let's put the TRIGGER_FOR_ADVANCED_COMMAND_MODE and
        // the default command name. 
        List<String> args = new ArrayList<>( Arrays.asList(in_args) );
        if ( args.size() == 0 || ! TRIGGER_FOR_ADVANCED_COMMAND_MODE.equals( args.get( 0 ))) {
            args.addAll( 0, Arrays.asList(  TRIGGER_FOR_ADVANCED_COMMAND_MODE, DEFAULT_COMMAND_NAME ) );
        } 

        // Remove the first element; at this point, it always equals to TRIGGER_FOR_ADVANCED_COMMAND_MODE. 
        args.remove(0);

        List<List<String>> arrayOfSubargs = 
                LamuBeginEndSplitter.splitBeginEnd( args, "begin",  "end" );

        List<ApplicationComponent> vessels = new ArrayList<>();
        Deque<Object> globalValueStack = new ArrayDeque<>();
        for (Iterator<List<String>> i = arrayOfSubargs.iterator(); i.hasNext();) {
            List<String> subargs = i.next();
            logInfo( subargs.toString() );
            parseSubargs(globalValueStack, availableCommands, vessels, subargs, false);
        }
        return vessels;
    }
}