package lamu;

import java.util.Collection;
import java.util.Deque;
import java.util.List;

import lamu.lib.app.ApplicationVessel;
import lamu.lib.app.process.JavaProcess;

class LamuCommandFork extends LamuCommand {
    static JavaProcess forkPulsar(List<String> arguments) {
        JavaProcess process = new JavaProcess(LamuApplication.class.getCanonicalName(), arguments);
        return process;
    }

    @Override
    boolean match(List<String> arguments) {
        return !arguments.isEmpty() && arguments.get(0).equals("fork");
    }

    @Override
    void execute( Collection<LamuCommand> availableCommands, Deque<ApplicationVessel> vessels, List<String> arguments, boolean recursiveCall) {
        List<String> subArguments = arguments.subList(1, arguments.size());
        // fork
        JavaProcess javaProcess = forkPulsar(subArguments);
        ApplicationVessel vessel = new ApplicationVessel( "ForkedVessel" );
        vessel.add( javaProcess );
        vessels.push( vessel );
    }
}