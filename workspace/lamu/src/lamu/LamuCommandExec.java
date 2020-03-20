package lamu;

import java.util.Collection;
import java.util.Deque;
import java.util.List;

import lamu.lib.app.ApplicationVessel;

class LamuCommandExec extends LamuCommand {
    @Override
    boolean match(List<String> arguments) {
        return !arguments.isEmpty() && arguments.get(0).equals("exec");
    }
    
    static <T> void setCollection( Collection<T> from, Collection<T> to ) {
        to.clear();
        to.addAll(from);
    }

    @Override
    void execute( Collection<LamuCommand> availableCommands, Deque<ApplicationVessel> vessels, List<String> arguments, boolean recursiveCall ) {
        
        List<String> subArguments = arguments.subList(1, arguments.size());
        // exec
        LamuApplicationArgumentParser argumentParser = new LamuApplicationArgumentParser();
        Deque<ApplicationVessel> valueStack = argumentParser.getValueStack( LamuApplicationArgumentParser.VESSELS );
        setCollection( vessels, valueStack );
        argumentParser.parse( subArguments );
        setCollection( valueStack, vessels );
    }
}