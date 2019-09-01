package kawapad;

import javax.swing.text.BadLocationException;

import kawapad.KawaPad.KawaPane;

final class RunnableInsertTextToTextPane implements Runnable {
		private KawaPane textPane;
		private final String result;
		RunnableInsertTextToTextPane( KawaPane textPane, String result ) {
			this.textPane = textPane;
			this.result = result;
		}
		
		@Override
		public void run() {
			KawaPad.logInfo( "InsertTextToTextPane() begin >>" );
			try {
				if ( textPane.getSelectedText() != null ) {
					try {
						textPane.getKawaPad().getUndoManager().startGroup();
						textPane.getKawaPad().getUndoManager().setSuspended(true);
						int selectionEnd = textPane.getSelectionEnd();
						textPane.getDocument().insertString( 
							selectionEnd, 
							result,
							null
//							((StyledEditorKit)textPane.getEditorKit()).getInputAttributes()
							);
						textPane.setSelectionEnd( selectionEnd + result.length() );
						textPane.setSelectionStart(selectionEnd  );
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
				textPane.getKawaPad().getKawaPane().updateHighlightLater();
			} catch (BadLocationException e1) {
				e1.printStackTrace();
			} finally { 
				KawaPad.logInfo( "InsertTextToTextPane() end <<" );
			}
		}
	}