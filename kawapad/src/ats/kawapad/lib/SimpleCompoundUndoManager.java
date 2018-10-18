package ats.kawapad.lib;

import javax.swing.event.UndoableEditEvent;
import javax.swing.undo.CannotRedoException;
import javax.swing.undo.CompoundEdit;
import javax.swing.undo.UndoableEdit;

public class SimpleCompoundUndoManager extends GroupedUndoManager {
	private static final boolean DEBUG_ADD_EDIT = false;
	private static final boolean DEBUG_SUSPENDED = false;
	protected transient boolean suspended = false;
	public SimpleCompoundUndoManager() {
		startGroup();
	}
	public synchronized void setSuspended(boolean suspended) {
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
	
	protected transient boolean validCompoundEdit = false;
	protected transient CompoundEdit compoundEdit = null;
	public synchronized void startGroup() {
		if ( DEBUG_SUSPENDED )
			if ( suspended  ) {
				System.err.println("startGroup() ... but suspended");
			} else {
				System.err.println();
				System.err.println("startGroup()");
			}

		if ( ! suspended ) {
			this.validCompoundEdit = false;
		}
	}
	
	@Override
	public synchronized void redo() throws CannotRedoException {
		try {
			setSuspended(true);
			super.redo();
		} finally {
			setSuspended(false);
			validCompoundEdit = false;
		}
	}
	@Override
	public synchronized void undo() throws CannotRedoException {
		try {
			setSuspended(true);
			System.err.println( "editToBeUndone() :"+ editToBeUndone() );
			super.undo();
		} finally {
			setSuspended(false);
			validCompoundEdit = false;
		}
	}
	
	@Override
	public synchronized boolean addEdit(UndoableEdit anEdit) {
		if ( DEBUG_ADD_EDIT )
			System.err.println( anEdit.getClass().getName() + ":" +  anEdit );

		if ( ! this.validCompoundEdit ) {
			if ( this.compoundEdit != null )
				this.compoundEdit.end();
			
			this.compoundEdit = new CompoundEdit();
			super.addEdit( this.compoundEdit );
			this.validCompoundEdit = true;
		}
		
		return super.addEdit( anEdit );
	}

	@Override
	public void undoableEditHappened(UndoableEditEvent e) {
		if ( DEBUG_ADD_EDIT )
			System.err.println( e.getEdit().getClass().getName() + ":" +  e.getEdit() );
		
		super.undoableEditHappened(e);
	}
	
}
