package lamu.lib.app.args;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;

public class ArgsCommandMacro extends ArgsCommand {
    public static ArgsCommandMacro create(String value) {
        ArrayList<String> list = new ArrayList<>( ArgsQuotedStringSplitter.splitString(value) );
        if (list.size() == 1 && list.get(0).trim().equals("")) {
            return null;
        }
        String macroName = list.remove(0);
        List<String> macroContent = list;
        Args.logInfo( String.format( "macro-from-config [%s]=>%s" , macroName, macroContent.toString() ) );
        return new ArgsCommandMacro(macroName, macroContent);
    }

    public static List<ArgsCommandMacro> load( Reader in ) throws IOException {
        List<ArgsCommandMacro> result = new ArrayList<>();
        try (BufferedReader r = new BufferedReader(in)) {
            for (;;) {
                String s = r.readLine();
                if (s == null)
                    break;
                if ( ! s.trim().startsWith(";"))
                    result.add(create(s));
            }
        }
        return result;
    }

    public static List<ArgsCommandMacro> load( File file ) throws IOException {
        if ( file.exists() && file.isFile() ) {
            try (FileReader f = new FileReader( file )) {
                return load(f);
            }
        } else {
            return Collections.emptyList();
        }
    }

    String macroName;
    List<String> macroContent;

    public ArgsCommandMacro(String macroName, List<String> macroContent) {
        super();
        this.macroName = macroName;
        this.macroContent = macroContent;
    }
    public String getMacroName() {
        return macroName;
    }
    public List<String> getMacroContent() {
        return macroContent;
    }
    @Override
    protected String commandName() {
        return this.getMacroName();
    }
    @Override
    protected void execute( ArgsCommandState state, List<String> arguments, int recursiveCount) {
        if ( Args.RECURSIVE_COUNT_MAX < recursiveCount ) {
            throw new Error( "a malformed default value in the default argument configuration." );
        }

        ArrayList<String> outArgs = new ArrayList<>();
        HashMap<String, ArgsNamedArgument> outNamedArgs = new HashMap<>();

        // parse the passed arguments
        Args.parseArguments( arguments, outArgs, outNamedArgs );

        // Execute the macro script.
        Args.executeMacro( state, getMacroName(), getMacroContent(), arguments, outArgs, outNamedArgs, recursiveCount );
    }


}