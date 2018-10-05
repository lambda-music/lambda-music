package ats.pulsar.editor.lib;

import javax.swing.undo.UndoableEdit;

public abstract class GroupedUndoManager extends UndoManager {
	public abstract void startGroup();
	public abstract void setSuspended(boolean suspended);
	public abstract boolean isSuspended();
	public void dump() {
		System.err.println( "==== DUMP ====" );
		
		for ( int i=0; i<edits.size(); i++  ) {
			System.err.println( edits.get(i) + ":" + (  indexOfNextAdd == i ? "[CURRENT]" : ""  ) );
		}
		System.err.println( "" );
	}
}
