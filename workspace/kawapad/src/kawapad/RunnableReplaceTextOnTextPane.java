package kawapad;

import javax.swing.text.BadLocationException;

final class RunnableReplaceTextOnTextPane implements Runnable {
    private Kawapad kawapad;
    private final String result;
    private boolean doReset;
    RunnableReplaceTextOnTextPane( Kawapad textPane, String result, boolean doReset ) {
        this.kawapad = textPane;
        this.result = result;
        this.doReset = doReset;
    }
    
    @Override
    public void run() {
        KawapadFrame.logInfo( "ReplaceTextOnTextPane() begin >>" );
        
        try {
            if ( kawapad.getSelectedText() != null ) {
                try {
                    kawapad.getUndoManager().startGroup();
                    kawapad.getUndoManager().setSuspended(true);
                    kawapad.replaceSelection( result );
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
                    kawapad.getCaret().moveDot(dot);
                } finally {
                    kawapad.getUndoManager().setSuspended(false);
                    kawapad.getUndoManager().endGroup();
                }
            }
            if ( doReset )
                kawapad.resetFileModifiedStatus();
            KawapadFrame.logInfo( "ReplaceTextOnTextPane() done" );
        } catch (BadLocationException e1) {
            e1.printStackTrace();
        } finally {
            KawapadFrame.logInfo( "ReplaceTextOnTextPane() end <<" );
        }
    }
}
