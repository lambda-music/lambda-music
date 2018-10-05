package ats.pulsar.editor.lib;

import javax.swing.SwingUtilities;
import javax.swing.undo.CannotRedoException;
import javax.swing.undo.CannotUndoException;
import javax.swing.undo.UndoableEdit;

public class InsignificantUndoManager extends GroupedUndoManager {
	private static final boolean DEBUG_ADD_EDIT = true;
	private static final boolean DEBUG_SUSPENDED = true;
	boolean suspended = false;
	public void setSuspended( boolean suspended ) {
		if ( DEBUG_SUSPENDED )
			if ( suspended ) {
				System.err.println();
				System.err.println("SUSPEND");
			} else {
				System.err.println("UNSUSPEND");
				System.err.println();
			}
		this.suspended = suspended;
	}
	public boolean isSuspended() {
		return suspended;
	}
	
	
    protected UndoableEdit editToBeRedone() {
    	UndoableEdit edit = super.editToBeRedone();
    	System.out.println("InsignificantUndoManager.editToBeRedone() : " + edit );
    	if ( edit != null )
    		return edit;
    	else
    		return edits.lastElement();
    }
    protected UndoableEdit editToBeUndone() {
    	UndoableEdit edit = super.editToBeUndone();
    	if ( edit != null )
    		return edit;
    	else
    		return edits.firstElement();
    }

	@Override
	public void startGroup() {
		if ( DEBUG_SUSPENDED )
			if ( suspended  ) {
				System.err.println("notifySignificant(suspended)");
			} else {
				System.err.println();
				System.err.println("notifySignificant");
			}

		if ( ! suspended ) {
			this.addEdit( new RelaxUndoableEdit("Significant", true ) );
		}
	}
	
	@Override
	public synchronized void redo() throws CannotRedoException {
		try {
			setSuspended(true);
			super.redo();
		} finally {
			SwingUtilities.invokeLater(new Runnable() {
				@Override
				public void run() {
					setSuspended(false);
				}
			});
		}
	}
	@Override
	public synchronized void undo() throws CannotRedoException {
		try {
			setSuspended(true);
			super.undo();
		} finally {
			SwingUtilities.invokeLater(new Runnable() {
				@Override
				public void run() {
					setSuspended(false);
				}
			});
		}
	}
	
	@Override
	public synchronized boolean addEdit(UndoableEdit anEdit) {
		if ( DEBUG_ADD_EDIT )
			System.err.println( anEdit.getClass().getName() + ":" +  anEdit );
		if ( anEdit instanceof RelaxUndoableEdit ) {
		} else {
			anEdit = new InsignificantUndoableEdit( anEdit );
		}
		return super.addEdit(anEdit);
	}
	
	static final class RelaxUndoableEdit implements UndoableEdit {
		private String presentationName;
		private String redoPresentationName;
		private String undoPresentationName;
		private boolean isSignificant;

		public RelaxUndoableEdit(String name, boolean isSignificant ) {
			super();
			this.presentationName = name;
			this.redoPresentationName = name;;
			this.undoPresentationName = name;;
			this.isSignificant = isSignificant;
		}

		public RelaxUndoableEdit(String presentationName, String redoPresentationName, String undoPresentationName, boolean isSignificant ) {
			super();
			this.presentationName = presentationName;
			this.redoPresentationName = redoPresentationName;
			this.undoPresentationName = undoPresentationName;
			this.isSignificant = isSignificant;
		}

		@Override
		public void undo() throws CannotUndoException {
		}
		@Override
		public boolean canUndo() {
			return ! this.isDead;
		}
		@Override
		public void redo() throws CannotRedoException {
		}
		@Override
		public boolean canRedo() {
			return ! this.isDead;
		}
		boolean isDead = false;
		@Override
		public void die() {
			this.isDead=true;
		}
		@Override
		public boolean addEdit(UndoableEdit anEdit) {
			return false;
		}
		@Override
		public boolean replaceEdit(UndoableEdit anEdit) {
			return false;
		}
		@Override
		public boolean isSignificant() {
			return isSignificant;
		}
		@Override
		public String getPresentationName() {
			return presentationName;
		}
		@Override
		public String getUndoPresentationName() {
			return undoPresentationName;
		}
		@Override
		public String getRedoPresentationName() {
			return redoPresentationName;
		}
		@Override
		public String toString() {
			return "RELAX:" + getPresentationName();
					
		}
	}
	
	static class UndoableEditDelegator implements UndoableEdit {
		UndoableEdit edit;
		public UndoableEditDelegator(UndoableEdit edit) {
			super();
			this.edit = edit;
		}
		public void undo() throws CannotUndoException {
			edit.undo();
		}
		public boolean canUndo() {
			return edit.canUndo();
		}
		public void redo() throws CannotRedoException {
			edit.redo();
		}
		public boolean canRedo() {
			return edit.canRedo();
		}
		public void die() {
			edit.die();
		}
		public boolean addEdit(UndoableEdit anEdit) {
			return edit.addEdit(anEdit);
		}
		public boolean replaceEdit(UndoableEdit anEdit) {
			return edit.replaceEdit(anEdit);
		}
		public boolean isSignificant() {
			return edit.isSignificant();
		}
		public String getPresentationName() {
			return edit.getPresentationName();
		}
		public String getUndoPresentationName() {
			return edit.getUndoPresentationName();
		}
		public String getRedoPresentationName() {
			return edit.getRedoPresentationName();
		}
	}
	
	static class InsignificantUndoableEdit extends UndoableEditDelegator {
		public InsignificantUndoableEdit(UndoableEdit edit) {
			super(edit);
		}
		@Override
		public boolean isSignificant() {
			return false;
		}
	}
	
}
