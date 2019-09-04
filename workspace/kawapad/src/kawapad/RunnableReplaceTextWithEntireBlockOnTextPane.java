package kawapad;

import javax.swing.text.BadLocationException;

final class RunnableReplaceTextWithEntireBlockOnTextPane implements Runnable {
    private Kawapad kawaPane;
    private final String result;
    RunnableReplaceTextWithEntireBlockOnTextPane( Kawapad textPane, String result ) {
        this.kawaPane = textPane;
        this.result = result;
    }
    
    @Override
    public void run() {
        KawapadFrame.logInfo( "ReplaceTextWithEntireBlockOnTextPane() begin >>" );
        
        try {
            if ( kawaPane.getSelectedText() != null ) {
                try {
                    kawaPane.getUndoManager().startGroup();
                    kawaPane.getUndoManager().setSuspended(true);
                    
                    // In order to avoid entering an infinite loop,
                    // we use /for/ loop instead of /while/ loop;
                    for ( int i=0; i<100; i++ ) {
                        if ( kawaPane.expandSelectedParentheses( kawaPane ) ) {
                            break;
                        }
                    }
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
            KawapadFrame.logInfo( "ReplaceTextWithEntireBlockOnTextPane() done" );
            kawaPane.updateHighlightLater();
        } catch (BadLocationException e1) {
            e1.printStackTrace();
        } finally {
            KawapadFrame.logInfo( "ReplaceTextWithEntireBlockOnTextPane() end <<" );
        }
    }
}
