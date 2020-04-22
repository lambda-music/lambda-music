package lamu.lib.args;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import lamu.lib.args.forking.ForkedProcess;
import lamu.lib.args.forking.ForkedProcessUtil;

public class ArgsCommandFork extends ArgsCommand {
    public interface ForkListener {
        void notifyForkedProcess( ArgsCommandState state, ForkedProcess process );
    }
    private final String name;
    private final Class<?> mainClass;
    private final ForkListener listener;
    public ArgsCommandFork(String name, Class<?> mainClass, ForkListener listener ) {
        super();
        this.name = name;
        this.mainClass = mainClass;
        this.listener = listener;
    }

    @Override
    protected String commandName() {
        return "fork";
    }

    @Override
    protected void execute( ArgsCommandState state, List<String> arguments, int recursiveCount) {
        List<String> fullArguments = new ArrayList<>();
        for ( Iterator<String> i = arguments.iterator();i.hasNext(); ) {
            String arg = i.next();
            if ( arg.equals( name ) ) {
                fullArguments.addAll( ForkedProcessUtil.getJavaArguments( mainClass.getCanonicalName() ) );
            } else {
                fullArguments.add( arg );
            }
        }
        
        // fork it
        ForkedProcess javaProcess = ForkedProcess.forkProcess( fullArguments );

//      Let it manually convert processes to streams (Tue, 24 Mar 2020 23:09:53 +0900)  
//        // Add the forked process to the streamables.
//        state.streamables.push( javaProcess );

        
        if ( listener != null ) {
            listener.notifyForkedProcess( state, javaProcess );
        }
    }
}