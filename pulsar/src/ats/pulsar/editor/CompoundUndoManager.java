package ats.pulsar.editor;

import javax.swing.JTextPane;
import javax.swing.event.UndoableEditEvent;
import javax.swing.undo.CannotRedoException;
import javax.swing.undo.CannotUndoException;
import javax.swing.undo.CompoundEdit;
import javax.swing.undo.UndoManager;
import javax.swing.undo.UndoableEdit;

public class CompoundUndoManager extends UndoManager {
	private JTextPane textPane;
	public CompoundUndoManager(JTextPane textPane) {
		this.textPane = textPane;
	}
	
	CompoundEdit compoundEdit = null;
	public void startCompoundEdit() {
		if ( compoundEdit != null )
			throw new IllegalStateException( "the compound edit was already started." );
		this.compoundEdit = new CompoundEditImplementation();
		this.addEdit(this.compoundEdit);
	}
	
	public void endCompoundEdit() {
		if ( compoundEdit == null )
			throw new IllegalStateException( "the compound edit is not started." );
		this.compoundEdit.end();
		this.compoundEdit = null;
	}
	
	@Override
	public synchronized boolean addEdit(UndoableEdit anEdit) {
		if ( anEdit instanceof RelaxUndoableEdit ) {
		} else {
			anEdit = new UnsignificantUndoableEdit( anEdit );
		}
		System.out.println( anEdit );
		return super.addEdit(anEdit);
	}
	
	boolean isEnabled = false;
	public void setEnabled( boolean isEnabled ) {
		this.isEnabled = isEnabled;
	}
	
	public static final class RelaxUndoableEdit implements UndoableEdit {
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
			return true;
//			return ! this.isDead;
		}
		@Override
		public void redo() throws CannotRedoException {
		}
		@Override
		public boolean canRedo() {
			return true;
//			return ! this.isDead;
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
	}
	
	static final class UnsignificantUndoableEdit implements UndoableEdit {
		UndoableEdit edit;
		public UnsignificantUndoableEdit(UndoableEdit edit) {
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
			return false;
//			return edit.isSignificant();
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
	
	@Override
	public void undoableEditHappened(UndoableEditEvent e) {
//		UndoableEdit edit = e.getEdit();
		super.undoableEditHappened(e);
		
//		if ( isEnabled ) {
//			if ( compoundEdit != null ) {
//				compoundEdit.addEdit(e.getEdit());
//			} else {
//				super.undoableEditHappened(e);
//			}
//		}
	}
	
	class CompoundEditImplementation extends CompoundEdit {
//		public boolean isInProgress() {
//			//  in order for the canUndo() and canRedo() methods to work
//			//  assume that the compound edit is never in progress
//
//			return false;
//		}
//		@Override
//		public boolean isSignificant() {
//			return true;
//		}

//		public void undo() throws CannotUndoException {
//			//  End the edit so future edits don't get absorbed by this edit
//
//			if (compoundEdit != null)
//				compoundEdit.end();
//
//			super.undo();
//
//			//  Always start a new compound edit after an undo
//			compoundEdit = null;
//		}
	}


}
