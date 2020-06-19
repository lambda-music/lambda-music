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
import java.util.ArrayList;
import java.util.logging.Level;

import javax.swing.event.DocumentEvent;
import javax.swing.event.UndoableEditEvent;
import javax.swing.text.AbstractDocument;
import javax.swing.text.BadLocationException;
import javax.swing.undo.CannotUndoException;
import javax.swing.undo.CompoundEdit;
import javax.swing.undo.UndoManager;
import javax.swing.undo.UndoableEdit;

import lamu.lib.log.Logger;


class MyCompoundEdit extends CompoundEdit {
    boolean isUnDone=false;
    public int getLength() {
        return edits.size();
    }

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
}

/**
 * See 
 * <a href="http://java-sl.com/tip_merge_undo_edits.html">http://java-sl.com/tip_merge_undo_edits.html</a><br/>
 * <a href="https://stackoverflow.com/a/34644544">https://stackoverflow.com/a/34644544</a><br/><br/>
 * 
 * (Fri, 19 Jun 2020 08:44:25 +0900)
 * 
 * See {@link EditOnlyUndoManager} for further information.
 *
 */

public class JavaSLGroupedUndoManager extends UndoManager implements GroupedUndoManager {
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
    
    String lastEditName=null;
    ArrayList<MyCompoundEdit> edits=new ArrayList<MyCompoundEdit>();
    MyCompoundEdit current;
    int pointer=-1;

    public void undoableEditHappened(UndoableEditEvent e) {
        UndoableEdit edit=e.getEdit();
        logInfo( "" + edit ); 
        if (edit instanceof AbstractDocument.DefaultDocumentEvent) {
            try {
                AbstractDocument.DefaultDocumentEvent event=(AbstractDocument.DefaultDocumentEvent)edit;
                
                // See https://stackoverflow.com/a/34644544
                if  (event.getType().equals(DocumentEvent.EventType.CHANGE)) {
                    super.undoableEditHappened(e);
                    return;
                }
                
                int start=event.getOffset();
                int len=event.getLength();
                String text=event.getDocument().getText(start, len);
                boolean isNeedStart=false;
                if (current==null) {
                    isNeedStart=true;
                }
                else if (text.contains("\n")) {
                    isNeedStart=true;
                }
                else if (lastEditName==null || !lastEditName.equals(edit.getPresentationName())) {
                    isNeedStart=true;
                }

                while (pointer<edits.size()-1) {
                    edits.remove(edits.size()-1);
                    isNeedStart=true;
                }
                if (isNeedStart) {
                    createCompoundEdit();
                }

                current.addEdit(edit);
                lastEditName=edit.getPresentationName();

                refreshControls();
            } catch (BadLocationException e1) {
                e1.printStackTrace();
            }
        }
    }

    public void createCompoundEdit() {
        if (current==null) {
            current= new MyCompoundEdit();
        }
        else if (current.getLength()>0) {
            current= new MyCompoundEdit();
        }

        edits.add(current);
        pointer++;
    }

    public void undo() throws CannotUndoException {
        if (!canUndo()) {
            throw new CannotUndoException();
        }

        MyCompoundEdit u=edits.get(pointer);
        u.undo();
        pointer--;

        refreshControls();
    }

    public void redo() throws CannotUndoException {
        if (!canRedo()) {
            throw new CannotUndoException();
        }

        pointer++;
        MyCompoundEdit u=edits.get(pointer);
        u.redo();

        refreshControls();
    }

    public boolean canUndo() {
        return pointer>=0;
    }

    private void refreshControls() {
        
    }

//    public boolean canRedo() {
//        return edits.size()>0 && pointer<edits.size()-1;
//    }
//
//    public void refreshControls() {
//        btnUndo.setEnabled(canUndo());
//        btnRedo.setEnabled(canRedo());
//    }    
    
}
