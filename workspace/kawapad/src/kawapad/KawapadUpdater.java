package kawapad;

import javax.swing.SwingUtilities;

import lamu.lib.log.SimpleConsole;
import lamu.lib.scheme.EvaluatorReceiver;
import lamu.lib.scheme.SchemeResult;

public class KawapadUpdater {
    public static EvaluatorReceiver create(
            Kawapad kawapad, boolean doInsertText,
            boolean doReplaceText, boolean doReportError, boolean doResetFileModified) 
    {
        return new InsertingUpdatingText( 
            kawapad, doInsertText, doReplaceText, doReportError, doResetFileModified );
    }
    
    static final class InsertingUpdatingText implements EvaluatorReceiver {
        private Kawapad kawapad;
        private boolean doInsertText;
        private boolean doReplaceText;
        private boolean doReportError;
        private boolean doResetFileModified;
        InsertingUpdatingText( 
                Kawapad kawapad,
                boolean doInsertText, 
                boolean doReplaceText, 
                boolean doReportError, 
                boolean doResetFileModified ) 
        {
            super();
            this.kawapad       = kawapad;
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
                // do not insert.
                Kawapad.logInfo( "KawapadEvaluator: do not insert (2). " + schemeResult.getValue() );
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
        public void receive( String schemeScript, SchemeResult schemeResult ) {
            if ( schemeResult.isSucceeded() ) {
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
                	SimpleConsole.getConsole().addText( schemeResult.getError());
                }
            }
        }
    }
}
