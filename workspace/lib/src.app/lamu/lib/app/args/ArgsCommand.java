package lamu.lib.app.args;

import java.util.List;

public abstract class ArgsCommand {
    protected abstract void    execute( ArgsState argsState, List<String> arguments, int recursiveCount );
    protected abstract String  commandName();
    protected          boolean match(ArgsState argsState, List<String> arguments) {
        return ! arguments.isEmpty() && arguments.get(0).equals( commandName() );
    }    
}