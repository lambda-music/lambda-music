package kawapad;

import javax.swing.SwingUtilities;

import pulsar.lib.scheme.SchemeExecutor;
import pulsar.lib.scheme.SchemeResult;

public abstract class KawapadEvaluatorRunnable implements Runnable {
    Kawapad kawapad;
    String schemeScript;
    boolean doInsertText;
    boolean doReplaceText;
    boolean doReportError;
    boolean doResetFileModified;
    KawapadEvaluatorRunnable( Kawapad kawapad, String schemeScript, boolean doInsertText, boolean doReplaceText, boolean doReportError, boolean doResetFileModified ) {
        super();
        this.kawapad = kawapad;
        this.schemeScript = schemeScript;
        this.doInsertText = doInsertText;
        this.doReplaceText = doReplaceText;
        this.doReportError = doReportError;
        this.doResetFileModified = doResetFileModified;
    }
    private void procDocument(SchemeResult schemeResult) {
        Kawapad.logWarn( "**KAWAPAD_PAGE**" );
        SwingUtilities.invokeLater( new RunnableReplaceTextWithEntireBlockOnTextPane(
            kawapad,
            schemeResult.valueAsString.replaceFirst( "\n$", "" ),
            false,
            doResetFileModified
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
            SwingUtilities.invokeLater( new RunnableInsertTextToTextPane( kawapad, resultString, true, doResetFileModified ) );
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
                doResetFileModified
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
            } else if ( doInsertText ) {
                if ( doReplaceText ) {
                    procReplace( schemeResult );
                } else {
                    procInsert( schemeResult );
                }
            } else {
                // do not insert if `insertText` is false.
                Kawapad.logInfo( "KawapadEvaluator: do not insert (3). " + schemeResult.value );
            }
        } else {
            // if error, insert anyway unless doReportError is false;
            if ( doReportError ) 
                procInsert( schemeResult );
        }
    }
    public abstract SchemeResult evaluate();
}