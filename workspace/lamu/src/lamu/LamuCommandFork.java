package lamu;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import lamu.lib.app.ApplicationVessel;
import lamu.lib.app.process.ForkedProcess;
import lamu.lib.app.process.ForkedProcessUtil;

class LamuCommandFork extends LamuCommand {
    @Override
    boolean match(State state, List<String> arguments) {
        return !arguments.isEmpty() && arguments.get(0).equals("fork");
    }

    @Override
    void execute( LamuCommand.State state, List<String> arguments, boolean recursiveCall) {
        List<String> fullArguments = new ArrayList<>();
        boolean first=true;
        for ( Iterator<String> i = arguments.iterator();i.hasNext(); ) {
            String arg = i.next();
            if ( first ) {
                first = false;
                continue;
            }
            
            if ( arg.equals( "lamu" ) ) {
                fullArguments.addAll( ForkedProcessUtil.getJavaArguments( LamuApplication.class.getCanonicalName() ) );
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