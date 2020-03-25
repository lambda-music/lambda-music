package lamu;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

class LamuCommandLoad extends LamuCommand {
    @Override
    protected String commandName() {
        return "load";
    }
    
    static <T> void setCollection( Collection<T> from, Collection<T> to ) {
        to.clear();
        to.addAll(from);
    }

    @Override
    protected void execute( LamuCommand.State state, List<String> arguments, boolean recursiveCall ) {
        List<String> subArguments = new ArrayList<>( arguments );
        if ( subArguments.isEmpty() ) {
            throw new Error( "no argument was specified" );
        } else {
            try {
                String uri = subArguments.remove(0);
                String content = new String(Files.readAllBytes( Paths.get(uri)), StandardCharsets.UTF_8 );
                List<String> scriptContent = LamuQuotedStringSplitter.splitString( content ); 
                
                LamuScript.execute( state, uri, scriptContent, subArguments  );

            } catch (IOException e) {
                throw new Error(e);
            }
        }
    }
}