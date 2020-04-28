package lamu.lib.args;

import java.util.List;

public abstract class ArgsCommandBuild extends ArgsCommand {
    private String commandName = "create";
    public ArgsCommandBuild(String commandName) {
        super();
        this.commandName = commandName;
    }
    @Override
    protected String commandName() {
        return commandName;
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