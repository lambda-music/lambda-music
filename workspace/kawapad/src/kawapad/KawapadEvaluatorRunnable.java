package kawapad;

import javax.swing.SwingUtilities;

import pulsar.lib.scheme.SchemeExecutor;
import pulsar.lib.scheme.SchemeResult;

public abstract class KawapadEvaluatorRunnable implements Runnable {
    Kawapad kawapad;
    String schemeScript;
    boolean insertText;
    boolean replaceText;
    boolean doReset;
    KawapadEvaluatorRunnable( Kawapad kawapad, String schemeScript, boolean insertText, boolean replaceText, boolean doReset ) {
        super();
        this.kawapad = kawapad;
        this.schemeScript = schemeScript;
        this.insertText = insertText;
        this.replaceText = replaceText;
        this.doReset = doReset;
    }
    private void procDocument(SchemeResult schemeResult) {
        Kawapad.logWarn( "**KAWAPAD_PAGE**" );
        SwingUtilities.invokeLater( new RunnableReplaceTextWithEntireBlockOnTextPane(
            kawapad,
            schemeResult.valueAsString.replaceFirst( "\n$", "" ),
            false,
            doReset
                ));
    }
    private void procInsert(SchemeResult schemeResult) {
        if ( ! schemeResult.isEmpty() ) {
            String resultString = SchemeExecutor.formatResult( schemeResult.valueAsString ); 
            // We want to make sure the result string ends with "\n" to avoid to get an extra line.
            if ( ! schemeScript.endsWith( "\n" ) ) {
                resultString = "\n" + SchemeExecutor.formatResult( schemeResult.valueAsString ); 
            }
            Kawapad.logInfo( resultString );
            SwingUtilities.invokeLater( new RunnableInsertTextToTextPane( kawapad, resultString, true, doReset ) );
        } else {
            // do not insert.
            Kawapad.logInfo( "KawapadEvaluator: do not insert (2). " + schemeResult.value );
        }
    }
    
    private void procReplace(SchemeResult schemeResult) {
        if ( ! schemeResult.isEmpty() ) {
            SwingUtilities.invokeLater( new RunnableReplaceTextOnTextPane(
                kawapad,
                schemeResult.valueAsString,
                doReset
                    ));
        } else {
            // do not insert.
            Kawapad.logInfo( "KawapadEvaluator: do not insert (1). " + schemeResult.value );
        }
    }
    
    
    @Override
    public void run() {
        Kawapad.logInfo( schemeScript );

        SchemeResult schemeResult = evaluate();
        
        if ( schemeResult.succeeded() ) {
            if ( schemeResult.isDocument ) {
                procDocument( schemeResult );
            } else if ( insertText ) {
                if ( replaceText ) {
                    procReplace( schemeResult );
                } else {
                    procInsert( schemeResult );
                }
            } else {
                // do not insert if `insertText` is false.
                Kawapad.logInfo( "KawapadEvaluator: do not insert (3). " + schemeResult.value );
            }
        } else {
            // if error, insert anyway. 
            procInsert( schemeResult );
        }
    }
    public abstract SchemeResult evaluate();
}