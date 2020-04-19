package lamu;

import java.util.Collection;
import java.util.List;

import lamu.lib.app.args.ArgsCommandCreate;
import lamu.lib.app.args.ArgsState;
import lamu.lib.app.args.ArgumentParser;

public class LamuCommandBuilder extends ArgsCommandCreate {
    @Override
    protected ArgumentParser create() {
        return new LamuApplicationArgumentParser();
    }
    
    static <T> void setCollection( Collection<T> from, Collection<T> to ) {
        to.clear();
        to.addAll(from);
    }
    
    @Override
    protected void execute( ArgsState argsState, List<String> arguments, int recursiveCount ) {
        // exec
        ArgumentParser argumentParser = create();
        setCollection( argsState.vessels,     argumentParser.getValueStack( LamuApplicationArgumentParser.VESSELS ) );
        setCollection( argsState.streamables, argumentParser.getValueStack( LamuApplicationArgumentParser.STREAMABLES ) );
        argumentParser.parse( arguments );
        setCollection( argumentParser.getValueStack( LamuApplicationArgumentParser.VESSELS ),     argsState.vessels  );
        setCollection( argumentParser.getValueStack( LamuApplicationArgumentParser.STREAMABLES ), argsState.streamables );
    }

}
