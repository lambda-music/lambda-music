package kawapad;

import javax.swing.text.BadLocationException;

final class RunnableInsertTextToTextPane implements Runnable {
    private Kawapad kawaPane;
    private final String result;
    RunnableInsertTextToTextPane( Kawapad textPane, String result ) {
        this.kawaPane = textPane;
        this.result = result;
    }
    
    @Override
    public void run() {
        KawapadFrame.logInfo( "InsertTextToTextPane() begin >>" );
        try {
            if ( kawaPane.getSelectedText() != null ) {
                try {
                    kawaPane.getUndoManager().startGroup();
                    kawaPane.getUndoManager().setSuspended(true);
                    int selectionEnd = kawaPane.getSelectionEnd();
                    kawaPane.getDocument().insertString( 
                        selectionEnd, 
                        result,
                        null
//                          ((StyledEditorKit)textPane.getEditorKit()).getInputAttributes()
                            );
                    kawaPane.setSelectionEnd( selectionEnd + result.length() );
                    kawaPane.setSelectionStart(selectionEnd  );
                } finally {
                    kawaPane.getUndoManager().setSuspended(false);
                    kawaPane.getUndoManager().endGroup();
                }
            } else {
                try {
                    kawaPane.getUndoManager().startGroup();
                    kawaPane.getUndoManager().setSuspended(true);
                    int dot = kawaPane.getCaret().getDot();
                    kawaPane.getDocument().insertString( dot, result, null);
                    kawaPane.getCaret().moveDot(dot);
                } finally {
                    kawaPane.getUndoManager().setSuspended(false);
                    kawaPane.getUndoManager().endGroup();
                }
            }
            kawaPane.updateHighlightLater();
        } catch (BadLocationException e1) {
            e1.printStackTrace();
        } finally { 
            KawapadFrame.logInfo( "InsertTextToTextPane() end <<" );
        }
    }
}
