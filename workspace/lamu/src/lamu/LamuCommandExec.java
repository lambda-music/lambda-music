package lamu;

import java.util.Deque;
import java.util.List;

import lamu.lib.app.ApplicationComponent;

class LamuCommandExec extends LamuCommand {
    @Override
    boolean match(List<String> arguments) {
        return !arguments.isEmpty() && arguments.get(0).equals("exec");
    }

    @Override
    void execute(Deque<Object> globalValueStack, List<LamuCommand> availableCommands, List<ApplicationComponent> vessels, List<String> arguments, boolean recursiveCall) {
        List<String> subArguments = arguments.subList(1, arguments.size());
        // exec
        LamuApplicationArgumentParser argumentParser = new LamuApplicationArgumentParser();
        Deque<Object> valueStack = argumentParser.getValueStack( LamuApplicationArgumentParser.GLOBAL_STACK );
        
        valueStack.clear();
        valueStack.addAll( globalValueStack );
        
        argumentParser.parse( subArguments );
        
        globalValueStack.clear();
        globalValueStack.addAll( valueStack );
        vessels.addAll( argumentParser.getApplicationVesselList());
    }
}