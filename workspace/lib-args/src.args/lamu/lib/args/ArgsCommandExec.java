package lamu.lib.args;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import lamu.lib.kawautils.SchemeValues;

public class ArgsCommandExec extends ArgsCommand {
    private String commandName = "exec";
    public ArgsCommandExec(String commandName) {
        super();
        this.commandName = commandName;
    }

    @Override
    protected String commandName() {
        return commandName;
    }
    
    static <T> void setCollection( Collection<T> from, Collection<T> to ) {
        to.clear();
        to.addAll(from);
    }

    @Override
    protected void execute( ArgsCommandState state, List<String> arguments, int recursiveCount ) {
        // Parse the arguments
        List<String> outSeqArgs = new ArrayList<>();
        Map<String, ArgsNamedArgument> outNamedArgs = new HashMap<>();
        Args.parseArguments( arguments, outSeqArgs, outNamedArgs);
        
        if (outSeqArgs.isEmpty() ) {
            
        } else {
            try {
                // Get the first argument as a filename and remove it.
                // Do no remove the current value because the current value should be used afterwards.
                String uri = outSeqArgs.get(0);

                // Read the file as a string value.
                String content = new String( Files.readAllBytes( Paths.get(uri)), StandardCharsets.UTF_8 );
                Object object = SchemeValues.string2lisp(content);
                String contentLisp;
                if ( object instanceof CharSequence ) {
                    contentLisp = SchemeValues.anyToString( object );
                } else {
                    // Currently support only string values.
                    throw new Error( "the first element of the file as a Lisp list must be a string value " + object );
                }

                // Parse the string value into a list of string values. 
                List<String> scriptContent = ArgsQuotedStringSplitter.splitString(contentLisp); 

                // Execute the string list as a script program.
                Args.executeMacro( state, uri, scriptContent, arguments, outSeqArgs, outNamedArgs, recursiveCount  );
            } catch (IOException e) {
                throw new Error(e);
            }
        }
    }
}