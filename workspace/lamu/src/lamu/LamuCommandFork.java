package lamu;

import java.util.List;

import lamu.lib.app.ApplicationVessel;
import lamu.lib.app.process.JavaProcess;

class LamuCommandFork extends LamuCommand {
    static JavaProcess forkPulsar(List<String> arguments) {
        JavaProcess process = new JavaProcess(LamuApplication.class.getCanonicalName(), arguments);
        return process;
    }

    @Override
    boolean match(State state, List<String> arguments) {
        return !arguments.isEmpty() && arguments.get(0).equals("fork");
    }

    @Override
    void execute( LamuCommand.State state, List<String> arguments, boolean recursiveCall) {
        List<String> subArguments = arguments.subList(1, arguments.size());
        // fork
        JavaProcess javaProcess = forkPulsar(subArguments);
        
        // Add the forked process to the streamables.
        state.streamables.push( javaProcess );

        // Create a vessel and put it to the vessel list.
        ApplicationVessel vessel = new ForkedApplicationVessel( "ForkedVessel" );
        vessel.add( javaProcess );
        state.vessels.push( vessel );
    }
}