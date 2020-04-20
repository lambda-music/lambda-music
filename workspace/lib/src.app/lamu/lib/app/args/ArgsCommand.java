package lamu.lib.app.args;

import java.util.List;

public abstract class ArgsCommand {
    protected abstract void    execute( ArgsBuilderState state, List<String> arguments, int recursiveCount );
    protected abstract String  commandName();
    protected          boolean match(ArgsBuilderState state, List<String> arguments) {
        return ! arguments.isEmpty() && arguments.get(0).equals( commandName() );
    }    
}