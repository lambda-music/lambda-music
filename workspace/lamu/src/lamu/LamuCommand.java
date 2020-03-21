package lamu;

import java.io.IOException;
import java.lang.invoke.MethodHandles;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Deque;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Level;

import lamu.lib.app.ApplicationVessel;
import lamu.lib.log.Logger;
import lamu.lib.stream.Streamable;

abstract class LamuCommand {
    static final Logger LOGGER = Logger.getLogger(MethodHandles.lookup().lookupClass().getName());
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg) { LOGGER.log(Level.INFO, msg); }
    static void logWarn(String msg) { LOGGER.log(Level.WARNING, msg); }

    abstract boolean match(   LamuCommand.State state, List<String> arguments );
    abstract void    execute( LamuCommand.State state, List<String> arguments, boolean recursiveCall );

    public static final class State {
        Collection<LamuCommand> availableCommands; 
        Deque<ApplicationVessel> vessels;
        Deque<Streamable> streamables;
        public State( Collection<LamuCommand> availableCommands ) {
            super();
            this.availableCommands = availableCommands;
            this.vessels = new ArrayDeque<>();
            this.streamables = new ArrayDeque<>();
        }
    }
    
    public static void parseSubargs( LamuCommand.State state, List<String> args, boolean isRecursiveCall ) {
        LamuCommand command = null;
        for (LamuCommand c : state.availableCommands ) {
            if ( c.match( state, args ) ) {
                command = c;
                break;
            }
        }

        if ( command != null ) {
            command.execute( state, args, isRecursiveCall );
        } else {
            // This should not happen because default command always matches.
            throw new Error("unknown command");
        }
    }

    static final String TRIGGER_FOR_ADVANCED_COMMAND_MODE = "do";
    static String DEFAULT_COMMAND_NAME = "default";

    public static void parseArgs( LamuCommand.State state, String[] in_args ) throws IOException {
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

        for (Iterator<List<String>> i = arrayOfSubargs.iterator(); i.hasNext();) {
            List<String> subargs = i.next();
            logInfo( subargs.toString() );
            parseSubargs( state, subargs, false );
        }
    }
}