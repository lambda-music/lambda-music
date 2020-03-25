package lamu;

import java.util.Collection;
import java.util.List;

class LamuCommandCreate extends LamuCommand {
    @Override
    protected String commandName() {
        return "create";
    }
    
    static <T> void setCollection( Collection<T> from, Collection<T> to ) {
        to.clear();
        to.addAll(from);
    }

    @Override
    protected void execute( LamuCommand.State state, List<String> arguments, boolean recursiveCall ) {
        // exec
        LamuApplicationArgumentParser argumentParser = new LamuApplicationArgumentParser();
        setCollection( state.vessels,     argumentParser.getValueStack( LamuApplicationArgumentParser.VESSELS ) );
        setCollection( state.streamables, argumentParser.getValueStack( LamuApplicationArgumentParser.STREAMABLES ) );
        argumentParser.parse( arguments );
        setCollection( argumentParser.getValueStack( LamuApplicationArgumentParser.VESSELS ),     state.vessels  );
        setCollection( argumentParser.getValueStack( LamuApplicationArgumentParser.STREAMABLES ), state.streamables );
    }
}