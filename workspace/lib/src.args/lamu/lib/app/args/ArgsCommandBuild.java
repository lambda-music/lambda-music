package lamu.lib.app.args;

import java.util.List;

public abstract class ArgsCommandBuild extends ArgsCommand {
    @Override
    protected String commandName() {
        return "create";
    }
    protected ArgsBuilder create() {
        return new ArgsBuilder();
    }
    protected abstract void initializeBuilder( ArgsCommandState state, ArgsBuilder builder );
    protected abstract void finalizeBuilder(   ArgsCommandState state, ArgsBuilder builder );
    @Override
    protected void execute( ArgsCommandState state, List<String> arguments, int recursiveCount ) {
        ArgsBuilder builder = create();
        initializeBuilder( state, builder );
        builder.parse( arguments );
        finalizeBuilder( state, builder );
        
    }
}