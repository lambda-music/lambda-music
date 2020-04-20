package lamu.lib.app.args;

import java.util.List;

public abstract class ArgsCommandBuild extends ArgsCommand {
    @Override
    protected String commandName() {
        return "create";
    }
    protected abstract ArgsBuilder create();

    @Override
    protected void execute( ArgsBuilderState state, List<String> arguments, int recursiveCount ) {
        ArgsBuilder argumentParser = create();
        argumentParser.parse( arguments );
    }
}