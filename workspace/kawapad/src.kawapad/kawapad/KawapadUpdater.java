package kawapad;

import javax.swing.SwingUtilities;

import lamu.lib.evaluators.EvaluatorReceiver;
import lamu.lib.evaluators.SchemeResult;

public class KawapadUpdater {
    public static EvaluatorReceiver create(
            Kawapad kawapad,
            String schemeScript,
            boolean doInsertText, boolean doReplaceText, boolean doReportError, boolean doResetFileModified ) 
    {
        return new InsertingUpdatingText( 
            kawapad, schemeScript, doInsertText, doReplaceText, doReportError, doResetFileModified );
    }
    
    static final class InsertingUpdatingText implements EvaluatorReceiver {
        private Kawapad kawapad;
        private String schemeScript;
        private boolean doInsertText;
        private boolean doReplaceText;
        private boolean doReportError;
        private boolean doResetFileModified;
        InsertingUpdatingText( 
                Kawapad kawapad,
                String schemeScript,
                boolean doInsertText, 
                boolean doReplaceText, 
                boolean doReportError, 
                boolean doResetFileModified ) 
        {
            super();
            this.kawapad       = kawapad;
            this.schemeScript  = schemeScript;
            this.doInsertText  = doInsertText;
            this.doReplaceText = doReplaceText;
            this.doReportError = doReportError;
            this.doResetFileModified = doResetFileModified;
        }
        private void procDocument(SchemeResult schemeResult) {
            Kawapad.logWarn( "**KAWAPAD_PAGE**" );
            SwingUtilities.invokeLater( new RunnableReplaceTextWithEntireBlockOnTextPane(
                kawapad,
                schemeResult.getValueAsString().replaceFirst( "\n$", "" ),
                false,
                doResetFileModified
                ));
        }
        private void procInsert( SchemeResult schemeResult, String schemeScript ) {
            if ( ! schemeResult.isEmpty() ) {
                String resultString = SchemeResult.formatResult( schemeResult.getValueAsString() ); 
                // We want to make sure the result string ends with "\n" to avoid to get an extra line.
                if ( ! schemeScript.endsWith( "\n" ) ) {
                    resultString = "\n" + SchemeResult.formatResult( schemeResult.getValueAsString() ); 
                }
//                Kawapad.logInfo( resultString );
                SwingUtilities.invokeLater( new RunnableInsertTextToTextPane( kawapad, resultString, true, doResetFileModified ) );
            } else {
                Kawapad.logInfo( "KawapadEvaluator: insert empty values (2). " + schemeResult.getValue() );
                // MODIFIED (Sun, 03 May 2020 05:23:18 +0900) >>>        
                // SwingUtilities.invokeLater( new RunnableInsertTextToTextPane( kawapad, "\n#| |#", true, doResetFileModified ) );
                SwingUtilities.invokeLater(   new RunnableInsertTextToTextPane( kawapad, "\n#|no-result|#", true, doResetFileModified ) );
                // MODIFIED (Sun, 03 May 2020 05:23:18 +0900) <<<        
//                // do not insert.
//                Kawapad.logInfo( "KawapadEvaluator: do not insert (2). " + schemeResult.getValue() );
            }
        }
        private void procReplace( SchemeResult schemeResult ) {
            if ( ! schemeResult.isEmpty() ) {
                SwingUtilities.invokeLater( new RunnableReplaceTextOnTextPane(
                    kawapad,
                    schemeResult.getValueAsString(),
                    doResetFileModified
                        ));
            } else {
                // do not insert.
                Kawapad.logInfo( "KawapadEvaluator: do not insert (1). " + schemeResult.getValue() );
            }
        }
        @Override
        public void receive( SchemeResult schemeResult ) {
            if ( schemeResult.isSucceeded() ) {

                // See  KawapadHistoryPair
                this.kawapad.addResultHistory( schemeResult.getValue() );
                
                if ( schemeResult.isDocument() ) {
                    procDocument( schemeResult );
                } else if ( doInsertText ) {
                    if ( doReplaceText ) {
                        procReplace( schemeResult );
                    } else {
                        procInsert( schemeResult, schemeScript );
                    }
                } else {
                    // do not insert if `insertText` is false.
                    Kawapad.logInfo( "KawapadEvaluator: do not insert (3). " + schemeResult.getValue() );
                }
            } else {
                // if error, insert anyway unless doReportError is false;
                if ( doReportError ) {
                    procInsert( schemeResult, schemeScript );
                    
                    // REMOVED (Fri, 24 Apr 2020 15:56:47 +0900)
                	// SimpleConsole.getConsole().addText( schemeResult.getError());
                }
            }
        }
    }
}
