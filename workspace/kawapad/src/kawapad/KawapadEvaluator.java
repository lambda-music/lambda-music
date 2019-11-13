package kawapad;

import java.io.File;
import java.util.HashMap;

import javax.swing.SwingUtilities;

import pulsar.lib.scheme.SchemeExecutor;
import pulsar.lib.scheme.scretary.SchemeSecretary;

public class KawapadEvaluator implements Runnable {
    public static KawapadEvaluator create(
            Kawapad kawapad, String schemeScript, File currentDirectory, File currentFile,
            boolean insertText, boolean replaceText, boolean doReset) 
    {
        return new KawapadEvaluator( kawapad, schemeScript, currentDirectory, currentFile, 
            insertText, replaceText, doReset );
    }


    Kawapad kawapad;
    String schemeScript;
    File currentFile;
    boolean insertText;
    boolean replaceText;
    boolean doReset;
    private File currentDirectory;
    private KawapadEvaluator(Kawapad kawapad, String schemeScript, File currentDirectory,
            File currentFile, boolean insertText, boolean replaceText, boolean doReset ) 
    {
        super();
        this.kawapad = kawapad;
        this.schemeScript = schemeScript;
        this.currentDirectory = currentDirectory;
        this.currentFile = currentFile;
        this.insertText = insertText;
        this.replaceText = replaceText;
        this.doReset = doReset;
    }
    private void procDocument(SchemeExecutor.Result result) {
        Kawapad.logWarn( "**KAWAPAD_PAGE**" );
        SwingUtilities.invokeLater( new RunnableReplaceTextWithEntireBlockOnTextPane(
            kawapad,
            result.valueAsString.replaceFirst( "\n$", "" ),
            false,
            doReset
            ));
    }
    private void procInsert(SchemeExecutor.Result result) {
        if ( ! result.isEmpty() ) {
            String resultString = SchemeExecutor.formatResult( result.valueAsString ); 
            // We want to make sure the result string ends with "\n" to avoid to get an extra line.
            if ( ! schemeScript.endsWith( "\n" ) ) {
                resultString = "\n" + SchemeExecutor.formatResult( result.valueAsString ); 
            }
            Kawapad.logInfo( resultString );
            SwingUtilities.invokeLater( new RunnableInsertTextToTextPane( kawapad, resultString, true, doReset ) );
        } else {
            // do not insert.
            Kawapad.logInfo( "KawapadEvaluator: do not insert (2). " + result.value );
        }
    }

    private void procReplace(SchemeExecutor.Result result) {
        if ( ! result.isEmpty() ) {
            SwingUtilities.invokeLater( new RunnableReplaceTextOnTextPane(
                kawapad,
                result.valueAsString,
                doReset
                    ));
        } else {
            // do not insert.
            Kawapad.logInfo( "KawapadEvaluator: do not insert (1). " + result.value );
        }
    }
    
    
    @Override
    public void run() {
        Kawapad.logInfo( schemeScript );
        HashMap<String,Object> variables = new HashMap<>();
        kawapad.initVariables( variables );
        SchemeExecutor.Result result = SchemeSecretary.evaluateScheme( 
            kawapad.schemeSecretary, 
            kawapad.getThreadInitializerList(), variables, 
            schemeScript, currentDirectory, currentFile, "scratchpad" );

        if ( result.succeeded() ) {
            if ( result.isDocument ) {
                procDocument( result );
            } else if ( insertText ) {
                if ( replaceText ) {
                    procReplace( result );
                } else {
                    procInsert( result );
                }
            } else {
                // do not insert if `insertText` is false.
                Kawapad.logInfo( "KawapadEvaluator: do not insert (3). " + result.value );
            }
        } else {
            // if error, insert anyway. 
            procInsert( result );
        }
    }
}