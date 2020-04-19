package lamu.lib.app.args;

import java.util.List;

public abstract class ArgsCommandCreate extends ArgsCommand {
    @Override
    protected String commandName() {
        return "create";
    }
    protected abstract ArgumentParser create();

    @Override
    protected void execute( ArgsState argsState, List<String> arguments, int recursiveCount ) {
        ArgumentParser argumentParser = create();
        argumentParser.parse( arguments );
    }
}