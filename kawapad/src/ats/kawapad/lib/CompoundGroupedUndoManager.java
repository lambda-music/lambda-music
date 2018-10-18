package ats.kawapad.lib;

import javax.swing.event.UndoableEditEvent;
import javax.swing.undo.CannotRedoException;
import javax.swing.undo.CompoundEdit;
import javax.swing.undo.UndoableEdit;

/**
 * (Fri, 05 Oct 2018 14:03:54 +0900)
 * 
 * I want to group these undoable edits but some of my attempts cause breaking
 * synchronization between undo edits and the actual document object. Whenever
 * this happens, the application freezes and destroys the currently editing
 * document. In my opinion, this is a really annoying bug.
 * 
 * In order to understand the problem correctly, it is very important to know
 * the fact that a calling addEdit() trims edits at the current position and
 * removes all the edit objects from the position.
 * 
 * It is necessary to create an instance of CompoundEdit whenever a user needs
 * to start a new group. The problem is that when to addEdit() the
 * instance.Because the calling addEdit() always causes a trim of edit objects.
 * 
 * It is necessary to start a new group (an instance of CompoundEdit) whenever
 * either undo() or redo() method is called. But because addEdit() always causes
 * a trim, it is not possible to add addEdit() beforehand.
 * 
 * This is the reason why `requestNewCompoundEdit` is necessary. The addEdit()
 * method should only be called whenever the user accept the trim; the only
 * occasion the user accept is starting new edit. Otherwise undo() redo()
 * process will never work correctly.
 * 
 * When it is necessary to start a new group, it is suffice to set
 * requestNewCompoundEdit to true. Whenever addEdit() is called, the method
 * checks the requestNewCompoundEdit flag and if it is true, it will create a
 * new CompoundEdit object and addEdit() to it.
 * 
 * This is the only one way which I found to get it work right. I could not find
 * any other way to work it right.
 * 
 * It seems that there is already a ready-made mechanism to do such process; if
 * the latest object added on an instance of CompoundEdit is an CompoundEdit,
 * CompoundEdit addEdit() method always tries to add the edit on the latest
 * added an UndoableEdit object.
 * 
 * But there is no mechanism to call end() method on the UndoableEdit inside an
 * UndoableEdit. The mechanism is necessary to specify when the group ends, but
 * it is missing.
 * 
 * I also happened to find some other potential bugs around isSignificant /
 * isInProgressing and other properties; I could not confirm that they are
 * actually bugs or not but I could not let them work right, neither.
 * 
 * I gave up to understand the problem. Only I know is that this
 * CompoundGroupedUndoManager works as my expectation. That's all.
 */


public class CompoundGroupedUndoManager extends GroupedUndoManager {
	private static final boolean DEBUG_ADD_EDIT = false;
	private static final boolean DEBUG_SUSPENDED = false;
	protected transient boolean suspended = false;
	public CompoundGroupedUndoManager() {
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
	
	
	protected transient CompoundEdit compoundEdit = null;
	protected transient boolean requestNewCompoundEdit = true;
	public synchronized void startGroup() {
		if ( DEBUG_SUSPENDED )
			if ( suspended  ) {
				System.err.println("startGroup() ... but suspended");
			} else {
				System.err.println();
				System.err.println("startGroup()");
			}

		if ( ! suspended ) {
			this.requestNewCompoundEdit = true;
		}
	}
	
	@Override
	public synchronized void redo() throws CannotRedoException {
		try {
			setSuspended(true);
			super.redo();
		} finally {
			setSuspended(false);
			requestNewCompoundEdit = true;
		}
	}
	@Override
	public synchronized void undo() throws CannotRedoException {
		try {
			setSuspended(true);
			// System.err.println( "editToBeUndone() :"+ editToBeUndone() );
			super.undo();
		} finally {
			setSuspended(false);
			requestNewCompoundEdit = true;
		}
	}
	
	@Override
	public synchronized boolean addEdit(UndoableEdit anEdit) {
		if ( DEBUG_ADD_EDIT )
			System.err.println( anEdit.getClass().getName() + ":" +  anEdit );

		if ( this.requestNewCompoundEdit || this.compoundEdit == null ) {
			if ( compoundEdit != null )
				compoundEdit.end();
			
			this.compoundEdit =  new CompoundEdit() {
				/*
				 * It is necessary to let isInProgress() always return true to make it work
				 * right. In fact, calling end() methods set the inProgress property to false,
				 * but by some reason calling end() method does not make it work right. Without
				 * this, an exception was thrown when the last undo edit is been undone.
				 */
				@Override
				public boolean isInProgress() {
					return false;
				}
//				@Override
//				public boolean isSignificant() {
//					return true;
//				}
//				{
//					this.end();
//				}
			};
			super.addEdit( compoundEdit );
			this.requestNewCompoundEdit = false;
		}
		
		return this.compoundEdit.addEdit( anEdit) ;
	}

	@Override
	public void undoableEditHappened(UndoableEditEvent e) {
		if ( DEBUG_ADD_EDIT )
			System.err.println( e.getEdit().getClass().getName() + ":" +  e.getEdit() );
		
		super.undoableEditHappened(e);
	}
	
}
