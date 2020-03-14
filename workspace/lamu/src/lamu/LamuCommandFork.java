package lamu;

import java.util.List;

import lamu.lib.app.ApplicationComponent;
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
    void execute(List<LamuCommand> availableCommands, List<ApplicationComponent> vessels, List<String> arguments, boolean recursiveCall) {
        List<String> subArguments = arguments.subList(1, arguments.size());
        // fork
        vessels.add( forkPulsar(subArguments) );
    }
}