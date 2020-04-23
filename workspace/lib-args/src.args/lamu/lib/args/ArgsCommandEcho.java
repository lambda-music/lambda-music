package lamu.lib.args;

import java.util.List;

public class ArgsCommandEcho extends ArgsCommand {
    private String commandName = "echo";
    public ArgsCommandEcho(String commandName) {
        super();
        this.commandName = commandName;
    }

    @Override
    protected String commandName() {
        return commandName;
    }

    @Override
    protected void execute( ArgsCommandState state, List<String> arguments, int recursiveCount) {
//        LamuScript.logInfo( String.format( "echo:%s", arguments.toString() ));
        System.out.println( String.join( " " , arguments ) );
    }
}