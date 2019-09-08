package kawapad;

import javax.swing.text.BadLocationException;

final class RunnableReplaceTextWithEntireBlockOnTextPane implements Runnable {
    private Kawapad kawapad;
    private final String result;
    private boolean doSelect;
    private boolean doReset;
    RunnableReplaceTextWithEntireBlockOnTextPane( Kawapad kawapad, String result, boolean doSelect, boolean doReset ) {
        this.kawapad = kawapad;
        this.result = result;
        this.doSelect = doSelect;
        this.doReset = doReset;
    }
    
    @Override
    public void run() {
        KawapadFrame.logInfo( "ReplaceTextWithEntireBlockOnTextPane() begin >>" );
        
        try {
            if ( kawapad.getSelectedText() != null ) {
                try {
                    kawapad.getUndoManager().startGroup();
                    kawapad.getUndoManager().setSuspended(true);
                    
                    // In order to avoid entering an infinite loop,
                    // we use /for/ loop instead of /while/ loop;
                    for ( int i=0; i<100; i++ ) {
                        if ( KawapadParenthesisMovement.expandSelectedParentheses( kawapad ) ) {
                            break;
                        }
                    }
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
                    if ( doSelect )
                        kawapad.getCaret().moveDot(dot);
                } finally {
                    kawapad.getUndoManager().setSuspended(false);
                    kawapad.getUndoManager().endGroup();
                }
            }
            if ( doReset )
                kawapad.resetFileModifiedStatus();
            KawapadFrame.logInfo( "ReplaceTextWithEntireBlockOnTextPane() done" );
        } catch (BadLocationException e1) {
            e1.printStackTrace();
        } finally {
            KawapadFrame.logInfo( "ReplaceTextWithEntireBlockOnTextPane() end <<" );
        }
    }
}
