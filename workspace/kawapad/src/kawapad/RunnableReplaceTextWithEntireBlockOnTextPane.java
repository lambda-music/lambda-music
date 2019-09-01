package kawapad;

import javax.swing.text.BadLocationException;

import kawapad.KawaPad.KawaPane;

final class RunnableReplaceTextWithEntireBlockOnTextPane implements Runnable {
		private KawaPane textPane;
		private final String result;
		RunnableReplaceTextWithEntireBlockOnTextPane( KawaPane textPane, String result ) {
			this.textPane = textPane;
			this.result = result;
		}
		
		@Override
		public void run() {
			KawaPad.logInfo( "ReplaceTextWithEntireBlockOnTextPane() begin >>" );

			try {
				if ( textPane.getSelectedText() != null ) {
					try {
						textPane.getKawaPad().getUndoManager().startGroup();
						textPane.getKawaPad().getUndoManager().setSuspended(true);

						// In order to avoid entering an infinite loop,
						// we use /for/ loop instead of /while/ loop;
						for ( int i=0; i<100; i++ ) {
							if ( textPane.getKawaPad().getKawaPane().expandSelectedParentheses( textPane ) ) {
								break;
							}
						}
						textPane.replaceSelection( result );
					} finally {
						textPane.getKawaPad().getUndoManager().setSuspended(false);
						textPane.getKawaPad().getUndoManager().endGroup();
					}
				} else {
					try {
						textPane.getKawaPad().getUndoManager().startGroup();
						textPane.getKawaPad().getUndoManager().setSuspended(true);
						int dot = textPane.getCaret().getDot();
						textPane.getDocument().insertString( dot, result, null);
						textPane.getCaret().moveDot(dot);
					} finally {
						textPane.getKawaPad().getUndoManager().setSuspended(false);
						textPane.getKawaPad().getUndoManager().endGroup();
					}
				}
				KawaPad.logInfo( "ReplaceTextWithEntireBlockOnTextPane() done" );
				textPane.getKawaPad().getKawaPane().updateHighlightLater();
			} catch (BadLocationException e1) {
				e1.printStackTrace();
			} finally {
				KawaPad.logInfo( "ReplaceTextWithEntireBlockOnTextPane() end <<" );
			}
		}
	}