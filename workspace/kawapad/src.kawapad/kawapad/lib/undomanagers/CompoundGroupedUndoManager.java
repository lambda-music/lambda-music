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


import java.lang.invoke.MethodHandles;
import java.util.logging.Level;

import javax.swing.event.DocumentEvent;
import javax.swing.event.UndoableEditEvent;
import javax.swing.text.AbstractDocument;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.undo.CannotRedoException;
import javax.swing.undo.CannotUndoException;
import javax.swing.undo.CompoundEdit;
import javax.swing.undo.UndoManager;
import javax.swing.undo.UndoableEdit;

import lamu.lib.logging.Logger;

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


public class CompoundGroupedUndoManager extends UndoManager implements GroupedUndoManager {
    public static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    public static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    public static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    public static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }

    private static final boolean DEBUG_ADD_EDIT = false;
    private static final boolean DEBUG_SUSPENDED = false;
    protected volatile boolean suspended = false;
    public CompoundGroupedUndoManager() {
        this.startGroup();
    }
    public synchronized void setSuspended(boolean suspended) {
        if ( DEBUG_ADD_EDIT )
            logInfo( "CompoundGroupedUndoManager.setSuspended()" +suspended );
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
    
    
    protected volatile CompoundEdit compoundEdit = null;
    protected volatile boolean requestNewCompoundEdit = true;
    public synchronized void startGroup0() {
        if ( DEBUG )
            logInfo( "CompoundGroupedUndoManager.startGroup() suspended=" + suspended );
        if ( ! suspended ) {
            this.requestNewCompoundEdit = true;
        }
    }
    
    @Override
    public synchronized void redo() throws CannotRedoException {
        if ( DEBUG_ADD_EDIT )
            logInfo( "CompoundGroupedUndoManager.redo()" );
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
        if ( DEBUG_ADD_EDIT )
            logInfo( "CompoundGroupedUndoManager.undo()" );
        try {
//          breakCurrentCompoundEdit();
            setSuspended(true);
            super.undo();
        } finally {
            setSuspended(false);
            requestNewCompoundEdit = true;
        }
    }
    
    @Override
    public synchronized boolean addEdit(UndoableEdit anEdit) {
        if ( DEBUG_ADD_EDIT )
            logInfo( "CompoundGroupedUndoManager.addEdit(): requestNewCompoundEdit="+ requestNewCompoundEdit );
        if ( DEBUG_ADD_EDIT )
            System.err.println( anEdit.getClass().getName() + ":" +  anEdit );

        breakCurrentCompoundEdit();
        
        return this.compoundEdit.addEdit( anEdit) ;
    }
    void breakCurrentCompoundEdit() {

        if ( this.requestNewCompoundEdit || this.compoundEdit == null ) {
            if ( compoundEdit != null )
                compoundEdit.end();
            
            this.compoundEdit =  new CompoundEdit() {
                boolean isUnDone=false;
         
                public void undo() throws CannotUndoException {
                    super.undo();
                    isUnDone=true;
                }
                public void redo() throws CannotUndoException {
                    super.redo();
                    isUnDone=false;
                }
                public boolean canUndo() {
                    return edits.size()>0 && !isUnDone;
                }

                public boolean canRedo() {
                    return edits.size()>0 && isUnDone;
                }
                        
                /*
                 * It is necessary to let isInProgress() always return true to make it work
                 * right. In fact, calling end() methods set the inProgress property to false,
                 * but by some reason calling end() method does not make it work right. Without
                 * this, an exception was thrown when the last undo edit is been undone.
                 */
//              @Override
//              public boolean isInProgress() {
//                  return false;
//              }
//              @Override
//              public boolean isSignificant() {
//                  return true;
//              }
//              {
//                  this.end();
//              }
            };
            super.addEdit( compoundEdit );
            this.requestNewCompoundEdit = false;
        }
    }

    @Override
    public synchronized void undoableEditHappened(UndoableEditEvent e) {
        if ( DEBUG_ADD_EDIT )
            logInfo( "CompoundGroupedUndoManager.undoableEditHappened()" );
        if ( DEBUG_ADD_EDIT )
            System.err.println( "IEH " + e.getEdit().getClass().getName() + ":" +  e.getEdit() );
        
        if ( e.getEdit() instanceof DefaultStyledDocument.AttributeUndoableEdit ) {
            if ( DEBUG_ADD_EDIT ) {
                logInfo( "rejected");
                logInfo( " ") ;
            }
            return;
        }
        if ( e.getEdit() instanceof AbstractDocument.DefaultDocumentEvent ) {
            if ( ((AbstractDocument.DefaultDocumentEvent)e.getEdit()).getType().equals( DocumentEvent.EventType.CHANGE ) ) {
                if ( DEBUG_ADD_EDIT ) {
                    logInfo( "rejected(change)");
                    logInfo( " ") ;
                }
                return;
            }
        }
        if ( DEBUG_ADD_EDIT ) {
            logInfo( "accepted()" );
            logInfo( " " ) ;
        }
        
        if ( DEBUG_ADD_EDIT )
            logInfo( " " ) ;
            
        super.undoableEditHappened(e);
    }
    ///////////////////////////////////////////////////////////////////////////////////////////////////

    public synchronized  void die() {
        if ( DEBUG_ADD_EDIT )
            logInfo( "CompoundGroupedUndoManager.die()" );
        super.die();
        if ( compoundEdit != null )
            compoundEdit.die();
    }
    public synchronized  void end() {
        if ( DEBUG_ADD_EDIT )
            logInfo( "CompoundGroupedUndoManager.end()" );
        super.end();
        compoundEdit.end();
    }
    public synchronized boolean canUndo() {
        if ( DEBUG_ADD_EDIT )
            logInfo( "CompoundGroupedUndoManager.canUndo()" );
        return super.canUndo();
//      return compoundEdit.canUndo();
    }
    public synchronized boolean canRedo() {
        if ( DEBUG_ADD_EDIT )
            logInfo( "CompoundGroupedUndoManager.canRedo()" );
        return canRedo();
//      return compoundEdit.canRedo();
    }
    public synchronized boolean isInProgress() {
        boolean result = super.isInProgress();
        if ( DEBUG_ADD_EDIT )
            logInfo( "CompoundGroupedUndoManager.isInProgress() : result="  + result );
        return result;
//      return compoundEdit.isInProgress();
    }
    public synchronized boolean replaceEdit(UndoableEdit anEdit) {
        if ( DEBUG_ADD_EDIT )
            logInfo( "CompoundGroupedUndoManager.replaceEdit()" );
        return replaceEdit( anEdit );
//      return compoundEdit.replaceEdit( anEdit );
    }
    public synchronized boolean isSignificant() {
        if ( DEBUG_ADD_EDIT )
            logInfo( "CompoundGroupedUndoManager.isSignificant()" );
        return super.isSignificant();
//      return compoundEdit.isSignificant();
    }
    public synchronized  String getPresentationName() {
        if ( DEBUG_ADD_EDIT )
            logInfo( "CompoundGroupedUndoManager.getPresentationName()" );
        return super.getPresentationName();
//      return compoundEdit.getPresentationName();
    }
    public synchronized String getUndoPresentationName() {
        if ( DEBUG_ADD_EDIT )
            logInfo( "CompoundGroupedUndoManager.getUndoPresentationName()" );
        return super.getUndoPresentationName();
//      return compoundEdit.getUndoPresentationName();
    }
    public synchronized String getRedoPresentationName() {
        if ( DEBUG_ADD_EDIT )
            logInfo( "CompoundGroupedUndoManager.getRedoPresentationName()" );
        return super.getRedoPresentationName();
//      return compoundEdit.getRedoPresentationName();
    }
    
    
    
}
