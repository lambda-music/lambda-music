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
import javax.swing.undo.UndoManager;
import javax.swing.undo.UndoableEdit;

import lamu.lib.logging.Logger;


/**
 * See {@link #undoableEditHappened0(UndoableEditEvent)} method:
 * The code is from <a href=
 * "https://stackoverflow.com/a/34644544">https://stackoverflow.com/a/34644544</a>.
 * This code lets UndoManager ignore CHANGE event which is generated when any
 * modification of attributes are made.
 * <p/>
 * This method does not work correctly. When it edits the document, it generates
 * an EditEvent event which contains information of a view which should be
 * removed but actually not. This causes a view is duplicately inserted to the
 * current document. This could devastate the document and when it occurs the
 * document may be lost.
 * <p/>
 * The solution which I found is overriding {@link #editToBeRedone()} and {@link #editToBeUndone()} to skip
 * the modification of attibutes. The problem is the necessary field {@link ClonedUndoManager#indexOfNextAdd} is 
 * defined with package private access modifier so it cannot be accessed.
 * <p/>
 * {@link ClonedUndoManager} is a class which is a copy-paste-clone of {@link UndoManager}. This is necessary
 * to expose the {@link ClonedUndoManager#indexOfNextAdd} field. 
 */

public class EditOnlyUndoManager extends ClonedUndoManager implements GroupedUndoManager {
    public static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    public static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    public static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    public static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }
    @Override
    public void startGroup0() {
    }
    @Override
    public void setSuspended(boolean suspended) {
    }
    @Override
    public boolean isSuspended() {
        return false;
    }

    synchronized void undoableEditHappened0(UndoableEditEvent e) {
        UndoableEdit edit=e.getEdit();
        // See https://stackoverflow.com/a/34644544
        if (edit instanceof AbstractDocument.DefaultDocumentEvent) {
            AbstractDocument.DefaultDocumentEvent event=(AbstractDocument.DefaultDocumentEvent)edit;
            logInfo( String.format( "sign:%s type:%s %s",  event.isSignificant() ,event.getType() , event ) );

            if  (event.getType().equals( DocumentEvent.EventType.CHANGE ) ) {
                return;
            }
        } else {
            logInfo( String.format( "sign:%s %s",  edit.isSignificant() ,edit) );
        }

        super.undoableEditHappened(e);
    }
    
//    This overridden method is removed. See undoableEditHappened0() .
//    @Override
//    public void undoableEditHappened(UndoableEditEvent e) {
//        undoableEditHappened0(e);
//    }
    
    
    /**
     * Returns the the next significant edit to be undone if <code>undo</code>
     * is invoked. This returns <code>null</code> if there are no edits
     * to be undone.
     *
     * @return the next significant edit to be undone
     */
    protected UndoableEdit editToBeUndone() {
        int i = indexOfNextAdd;
        while (i > 0) {
            UndoableEdit edit = edits.elementAt(--i);
            if (edit.isSignificant() && /*ADDED June 19, 2020 */ ! isChangeEvent( edit ) ) {
                return edit;
            }
        }

        return null;
    }

    /**
     * Returns the the next significant edit to be redone if <code>redo</code>
     * is invoked. This returns <code>null</code> if there are no edits
     * to be redone.
     *
     * @return the next significant edit to be redone
     */
    protected UndoableEdit editToBeRedone() {
        int count = edits.size();
        int i = indexOfNextAdd;

        while (i < count) {
            UndoableEdit edit = edits.elementAt(i++);
            if (edit.isSignificant() && /*ADDED June 19, 2020 */ ! isChangeEvent( edit ) ) {
                return edit;
            }
        }

        return null;
    }
    
    public static final boolean isChangeEvent( UndoableEdit edit ) {
        if (edit instanceof AbstractDocument.DefaultDocumentEvent) {
            AbstractDocument.DefaultDocumentEvent event=(AbstractDocument.DefaultDocumentEvent)edit;
            if  (event.getType().equals( DocumentEvent.EventType.CHANGE ) ) {
                return true;
            }
        }
        return false;
    }
    
}
