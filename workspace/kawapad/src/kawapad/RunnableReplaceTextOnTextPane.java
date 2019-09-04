package kawapad;

import javax.swing.text.BadLocationException;

final class RunnableReplaceTextOnTextPane implements Runnable {
    private Kawapad kawaPane;
    private final String result;
    RunnableReplaceTextOnTextPane( Kawapad textPane, String result ) {
        this.kawaPane = textPane;
        this.result = result;
    }
    
    @Override
    public void run() {
        KawapadFrame.logInfo( "ReplaceTextOnTextPane() begin >>" );
        
        try {
            if ( kawaPane.getSelectedText() != null ) {
                try {
                    kawaPane.getUndoManager().startGroup();
                    kawaPane.getUndoManager().setSuspended(true);
                    kawaPane.replaceSelection( result );
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
            KawapadFrame.logInfo( "ReplaceTextOnTextPane() done" );
            kawaPane.updateHighlightLater();
        } catch (BadLocationException e1) {
            e1.printStackTrace();
        } finally {
            KawapadFrame.logInfo( "ReplaceTextOnTextPane() end <<" );
        }
    }
}
