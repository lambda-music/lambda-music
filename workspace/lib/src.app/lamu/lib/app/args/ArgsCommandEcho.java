package lamu.lib.app.args;

import java.util.List;

public class ArgsCommandEcho extends ArgsCommand {
    @Override
    protected String commandName() {
        return "echo";
    }

    @Override
    protected void execute( ArgsBuilderState state, List<String> arguments, int recursiveCount) {
//        LamuScript.logInfo( String.format( "echo:%s", arguments.toString() ));
        System.out.println( String.join( " " , arguments ) );
    }
}