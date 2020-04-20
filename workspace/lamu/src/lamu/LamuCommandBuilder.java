package lamu;

import static lamu.LamuApplicationBuilder.STREAMABLES;
import static lamu.LamuApplicationBuilder.VESSELS;

import java.util.Collection;

import lamu.lib.app.args.ArgsBuilder;
import lamu.lib.app.args.ArgsCommandBuild;
import lamu.lib.app.args.ArgsCommandState;

public class LamuCommandBuilder extends ArgsCommandBuild {
    static <T> void setCollection( Collection<T> from, Collection<T> to ) {
        to.clear();
        to.addAll(from);
    }
    @Override
    protected void initializeBuilder(ArgsCommandState state, ArgsBuilder builder) {
        LamuApplicationBuilder.initializeBuilder( builder );
        setCollection( state.vessels,     builder.getValueStack( VESSELS ) );
        setCollection( state.streamables, builder.getValueStack( STREAMABLES ) );
    }
    @Override
    protected void finalizeBuilder(ArgsCommandState state, ArgsBuilder builder) {
        LamuApplicationBuilder.finalizeBuilder( state, builder);
        setCollection( builder.getValueStack( VESSELS ),     state.vessels  );
        setCollection( builder.getValueStack( STREAMABLES ), state.streamables );
    }
}
