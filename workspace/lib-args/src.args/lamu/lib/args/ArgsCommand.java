package lamu.lib.args;

import java.util.List;

public abstract class ArgsCommand {
    protected abstract void    execute( ArgsCommandState state, List<String> arguments, int recursiveCount );
    protected abstract String  commandName();
    protected          boolean match(ArgsCommandState state, List<String> arguments) {
        return ! arguments.isEmpty() && arguments.get(0).equals( commandName() );
    }    
}