package lamu;

import java.util.Collection;
import java.util.List;

import lamu.lib.app.args.ArgsCommandBuild;
import lamu.lib.app.args.ArgsBuilderState;
import lamu.lib.app.args.ArgsBuilder;

public class LamuCommandBuilder extends ArgsCommandBuild {
    @Override
    protected ArgsBuilder create() {
        return new LamuApplicationArgumentParser();
    }
    
    static <T> void setCollection( Collection<T> from, Collection<T> to ) {
        to.clear();
        to.addAll(from);
    }
    
    @Override
    protected void execute( ArgsBuilderState state, List<String> arguments, int recursiveCount ) {
        // exec
        ArgsBuilder argumentParser = create();
        setCollection( state.vessels,     argumentParser.getValueStack( LamuApplicationArgumentParser.VESSELS ) );
        setCollection( state.streamables, argumentParser.getValueStack( LamuApplicationArgumentParser.STREAMABLES ) );
        argumentParser.parse( arguments );
        setCollection( argumentParser.getValueStack( LamuApplicationArgumentParser.VESSELS ),     state.vessels  );
        setCollection( argumentParser.getValueStack( LamuApplicationArgumentParser.STREAMABLES ), state.streamables );
    }

}
