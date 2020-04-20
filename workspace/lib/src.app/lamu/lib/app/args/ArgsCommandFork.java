package lamu.lib.app.args;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import lamu.lib.app.ApplicationVessel;
import lamu.lib.app.process.ForkedProcess;
import lamu.lib.app.process.ForkedProcessUtil;

public class ArgsCommandFork extends ArgsCommand {
    private final String name;
    private final Class<?> mainClass;
    public ArgsCommandFork(String name, Class<?> mainClass) {
        super();
        this.name = name;
        this.mainClass = mainClass;
    }

    @Override
    protected String commandName() {
        return "fork";
    }

    @Override
    protected void execute( ArgsBuilderState state, List<String> arguments, int recursiveCount) {
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

        // Create a vessel and put it to the vessel list.
        ApplicationVessel vessel = new ApplicationVessel( "ForkedVessel" );
        vessel.add( javaProcess );
        
        // Push it to the stack for vessels.
        state.vessels.push( vessel );
    }
}