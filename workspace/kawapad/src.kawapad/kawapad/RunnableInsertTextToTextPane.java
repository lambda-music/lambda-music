package kawapad;

import javax.swing.text.BadLocationException;

final class RunnableInsertTextToTextPane implements Runnable {
    private Kawapad kawapad;
    private final String result;
    private boolean doSelect;
    private boolean doReset;
    RunnableInsertTextToTextPane( Kawapad textPane, String result, boolean doSelect, boolean doReset ) {
        this.kawapad = textPane;
        this.result = result;
        this.doSelect = doSelect;
        this.doReset = doReset;
    }
    
    @Override
    public void run() {
        KawapadFrame.logInfo( "InsertTextToTextPane() begin >>" );
        try {
            if ( kawapad.getSelectedText() != null ) {
                try {
                    kawapad.getUndoManager().startGroup();
                    kawapad.getUndoManager().setSuspended(true);
                    int selectionEnd = kawapad.getSelectionEnd();
                    kawapad.getDocument().insertString( 
                        selectionEnd, 
                        result,
                        null
//                          ((StyledEditorKit)textPane.getEditorKit()).getInputAttributes()
                            );
                    kawapad.setSelectionEnd( selectionEnd + result.length() );
                    kawapad.setSelectionStart(selectionEnd  );
                } finally {
                    kawapad.getUndoManager().setSuspended(false);
                    kawapad.getUndoManager().endGroup();
                }
            } else {
                try {
                    kawapad.getUndoManager().startGroup();
                    kawapad.getUndoManager().setSuspended(true);
                    int dot = kawapad.getCaret().getDot();
                    kawapad.getDocument().insertString( dot, result, null);
                    if ( doSelect )
                        kawapad.getCaret().moveDot(dot);
                } finally {
                    kawapad.getUndoManager().setSuspended(false);
                    kawapad.getUndoManager().endGroup();
                }
            }
            if ( doReset )
                kawapad.resetFileModifiedStatus();
        } catch (BadLocationException e1) {
            e1.printStackTrace();
        } finally { 
            KawapadFrame.logInfo( "InsertTextToTextPane() end <<" );
        }
    }
}
