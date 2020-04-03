package lamu;

import java.util.List;

class LamuCommandEcho extends LamuCommand {
    @Override
    protected String commandName() {
        return "echo";
    }

    @Override
    protected void execute( LamuScript.State state, List<String> arguments, int recursiveCount) {
//        LamuScript.logInfo( String.format( "echo:%s", arguments.toString() ));
        System.out.println( String.join( " " , arguments ) );
    }
}