package lamu;

import java.util.Collection;
import java.util.List;

class LamuCommandCreate extends LamuCommand {
    @Override
    boolean match(State state, List<String> arguments) {
        return !arguments.isEmpty() && arguments.get(0).equals( "create" );
    }
    
    static <T> void setCollection( Collection<T> from, Collection<T> to ) {
        to.clear();
        to.addAll(from);
    }

    @Override
    void execute( LamuCommand.State state, List<String> arguments, boolean recursiveCall ) {
        
        List<String> subArguments = arguments.subList(1, arguments.size());
        // exec
        LamuApplicationArgumentParser argumentParser = new LamuApplicationArgumentParser();
        setCollection( state.vessels,     argumentParser.getValueStack( LamuApplicationArgumentParser.VESSELS ) );
        setCollection( state.streamables, argumentParser.getValueStack( LamuApplicationArgumentParser.STREAMABLES ) );
        argumentParser.parse( subArguments );
        setCollection( argumentParser.getValueStack( LamuApplicationArgumentParser.VESSELS ),     state.vessels  );
        setCollection( argumentParser.getValueStack( LamuApplicationArgumentParser.STREAMABLES ), state.streamables );
    }
}