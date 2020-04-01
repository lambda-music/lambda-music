package lamu;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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
    protected void execute( LamuScript.State state, List<String> arguments, boolean recursiveCall ) {
        // Parse the arguments
        List<String> outSeqArgs = new ArrayList<>();
        Map<String, LamuNamedArgument> outNamedArgs = new HashMap<>();
        LamuScript.parseMacro( arguments, outSeqArgs, outNamedArgs);
        
        try {
            // Get the first argument as a filename and remove it.
            String uri = outSeqArgs.remove(0);
            
            // Read the file as a string value.
            String content = new String( Files.readAllBytes( Paths.get(uri)), StandardCharsets.UTF_8 );
            
            // Parse the string value into a list of string values. 
            List<String> scriptContent = LamuQuotedStringSplitter.splitString( content ); 
            
            // Execute the string list as a script program.
            LamuScript.executeMacro( state, uri, scriptContent, arguments, outSeqArgs, outNamedArgs  );

        } catch (IOException e) {
            throw new Error(e);
        }

        if ( arguments.isEmpty() ) {
            throw new Error( "no argument was specified" );
        }
    }
}