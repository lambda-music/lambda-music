package lamu;

import java.io.IOException;
import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;

import lamu.lib.app.args.Args;
import lamu.lib.app.args.ArgsCommandState;
import lamu.lib.app.args.ArgsNamedArgument;
import lamu.lib.log.Logger;

public class LamuScript {
    static final String TRIGGER_FOR_ADVANCED_COMMAND_MODE = "advanced";
    static final String DEFAULT_COMMAND_OPEN = "open";
    static final String DEFAULT_COMMAND_EXEC = "exec";
    static final String DEFAULT_COMMAND_LOAD = "load";
    static final String DEFAULT_COMMAND = "default";
    
    static final Logger LOGGER = Logger.getLogger(MethodHandles.lookup().lookupClass().getName());
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg) { LOGGER.log(Level.INFO, msg); }
    static void logWarn(String msg) { LOGGER.log(Level.WARNING, msg); }
    
    public static void parse( ArgsCommandState state, String[] args ) throws IOException {
        executeScript0( state, new ArrayList<>( Arrays.asList(args)) );
    }
    public static void executeScript0(ArgsCommandState state, List<String> arguments) {
        // Put a proper default command.
        defaultCommandInterpolation( arguments ); 

        Args.executeScript( state, arguments );
    }

    /**
     * Perform the default command interpolation on the passed argument list.
     * 
     * If the first element is not TRIGGER_FOR_ADVANCED_COMMAND_MODE, regard it as
     * the "default mode". Let's put the TRIGGER_FOR_ADVANCED_COMMAND_MODE and a
     * propert default command name which fits to the condition.
     * 
     * @param arguments
     *    the target argument list
     */
    static void defaultCommandInterpolation(List<String> arguments) {
        if ( arguments.size() == 0 || ! TRIGGER_FOR_ADVANCED_COMMAND_MODE.equals( arguments.get( 0 ))) {
            List<String> outSeqArgs = new ArrayList<>();
            Map<String, ArgsNamedArgument> outNamedArgs = new HashMap<>();
            Args.parseArguments( arguments, outSeqArgs, outNamedArgs );

            if ( false ) {
                // 
            } else if ( outNamedArgs.containsKey( "command" ) ) {
                // --command=[command-name] invokes a specific command   
                String commandName = outNamedArgs.get( "command" ).getValue();
                logInfo( "default mode command : --command=" + commandName );
                arguments.addAll( 0, Arrays.asList(  TRIGGER_FOR_ADVANCED_COMMAND_MODE, commandName ) );
                
            } else if ( outNamedArgs.containsKey( "exec" ) || outNamedArgs.containsKey( "e" ) ) {
                // --exec/-e are equivalent to --command=exec
                logInfo( "default mode command : --exec" );
                arguments.addAll( 0, Arrays.asList(  TRIGGER_FOR_ADVANCED_COMMAND_MODE, DEFAULT_COMMAND_EXEC ) );
                
            } else if ( outNamedArgs.containsKey( "load" )) {
                // --load is equivalent to --command=load. The purpos of this command is mainly
                // to support shebang. Shebang accepts only one command-line argument with only one filename;
                // shebang cannot accept command-line arguments more than two.
                
                String fileName = outNamedArgs.get( "load" ).getValue();
                logInfo( "default mode command : --load=" + fileName );
                arguments.addAll( 0, Arrays.asList(  TRIGGER_FOR_ADVANCED_COMMAND_MODE, DEFAULT_COMMAND_LOAD, fileName ) );
                
            } else {
                // The default command.
                arguments.addAll( 0, Arrays.asList(  TRIGGER_FOR_ADVANCED_COMMAND_MODE, DEFAULT_COMMAND ) );
            }
        }
        

        // Remove the first element; at this point, it always equals to TRIGGER_FOR_ADVANCED_COMMAND_MODE. 
        if ( ! TRIGGER_FOR_ADVANCED_COMMAND_MODE.equals( arguments.remove(0))) {
            throw new Error( "internal error" );
        };
    }

}
