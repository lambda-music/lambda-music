package lamu;

import java.util.List;

abstract class LamuCommand {
    protected abstract void    execute( LamuScript.State state, List<String> arguments, int recursiveCount );
    protected abstract String  commandName();
    protected          boolean match(LamuScript.State state, List<String> arguments) {
        return ! arguments.isEmpty() && arguments.get(0).equals( commandName() );
    }    
}