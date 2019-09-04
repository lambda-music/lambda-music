/*
 * Kawapad written by Atsushi Oka 
 * Copyright 2018 Atsushi Oka
 *
 * This file is part of Metro Musical Sequencing Framework. 
 * 
 * Kawapad is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * Kawapad is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with Kawapad.  If not, see <https://www.gnu.org/licenses/>.
 */

package kawapad.lib.undomanagers;

import javax.swing.event.UndoableEditEvent;
import javax.swing.undo.CannotRedoException;
import javax.swing.undo.CompoundEdit;
import javax.swing.undo.UndoableEdit;

public class SimpleCompoundUndoManager extends UndoManager implements GroupedUndoManager {
    private static final boolean DEBUG_ADD_EDIT = false;
    private static final boolean DEBUG_SUSPENDED = false;
    protected transient boolean suspended = false;
    public SimpleCompoundUndoManager() {
        this.startGroup();
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
    public synchronized void startGroup0() {
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
