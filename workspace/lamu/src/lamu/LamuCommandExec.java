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

import gnu.lists.IString;
import lamu.lib.scheme.SchemeUtils;

class LamuCommandExec extends LamuCommand {
    @Override
    protected String commandName() {
        return "exec";
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
        LamuScript.parseArguments( arguments, outSeqArgs, outNamedArgs);
        
        try {
            // Get the first argument as a filename and remove it.
            // Do no remove the current value because the current value should be used afterwards.
            String uri = outSeqArgs.get(0);
            
            // Read the file as a string value.
            String content = new String( Files.readAllBytes( Paths.get(uri)), StandardCharsets.UTF_8 );
            Object object = SchemeUtils.string2scheme(content);
            String contentLisp;
            if ( object instanceof String ) {
                contentLisp = (String) object;
            } else if ( object instanceof IString ) {
                    contentLisp = SchemeUtils.schemeStringToJavaString(object);
            } else {
                // Currently support only string values.
                throw new Error( "the first element of the file as a Lisp list must be a string value" );
            }

            // Parse the string value into a list of string values. 
            List<String> scriptContent = LamuQuotedStringSplitter.splitString( contentLisp ); 
            
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