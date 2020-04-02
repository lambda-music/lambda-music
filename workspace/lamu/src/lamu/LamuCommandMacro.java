package lamu;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;

class LamuCommandMacro extends LamuCommand {
    static LamuCommandMacro create(String value) {
        ArrayList<String> list = new ArrayList<>( LamuQuotedStringSplitter.splitString(value) );
        if (list.size() == 1 && list.get(0).trim().equals("")) {
            return null;
        }
        String macroName = list.remove(0);
        List<String> macroContent = list;
        LamuApplication.logInfo( String.format( "macro-from-config [%s]=>%s" , macroName, macroContent.toString() ) );
        return new LamuCommandMacro(macroName, macroContent);
    }

    static List<LamuCommandMacro> load(Reader in) throws IOException {
        List<LamuCommandMacro> result = new ArrayList<>();
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

    static List<LamuCommandMacro> load( File file ) throws IOException {
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

    public LamuCommandMacro(String macroName, List<String> macroContent) {
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
    protected void execute( LamuScript.State state, List<String> arguments, boolean recursiveCall) {
        if (recursiveCall) {
            throw new Error( "a malformed default value in the default argument configuration." );
        }

        ArrayList<String> outArgs = new ArrayList<>();
        HashMap<String, LamuNamedArgument> outNamedArgs = new HashMap<>();

        // parse the passed arguments
        LamuScript.parseArguments( arguments, outArgs, outNamedArgs );

        // Execute the macro script.
        LamuScript.executeMacro( state, getMacroName(), getMacroContent(), arguments, outArgs, outNamedArgs );
    }


}