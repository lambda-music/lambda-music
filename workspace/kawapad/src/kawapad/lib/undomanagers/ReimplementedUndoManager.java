package kawapad.lib.undomanagers;

import java.util.ArrayList;

import javax.swing.event.UndoableEditEvent;
import javax.swing.event.UndoableEditListener;
import javax.swing.text.AbstractDocument;
import javax.swing.text.BadLocationException;
import javax.swing.undo.AbstractUndoableEdit;
import javax.swing.undo.CannotUndoException;
import javax.swing.undo.CompoundEdit;
import javax.swing.undo.UndoableEdit;



class RumCompoundEdit extends CompoundEdit {
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
public class ReimplementedUndoManager extends AbstractUndoableEdit implements UndoableEditListener, GroupedUndoManager {
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
    @Override
    public void discardAllEdits() {
    }

    String lastEditName=null;
    ArrayList<RumCompoundEdit> edits=new ArrayList<RumCompoundEdit>();
    RumCompoundEdit current;
    int pointer=-1;

    public void undoableEditHappened(UndoableEditEvent e) {
        UndoableEdit edit=e.getEdit();
        if (edit instanceof AbstractDocument.DefaultDocumentEvent) {
            try {
                AbstractDocument.DefaultDocumentEvent event=(AbstractDocument.DefaultDocumentEvent)edit;
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
            current= new RumCompoundEdit();
        }
        else if (current.getLength()>0) {
            current= new RumCompoundEdit();
        }

        edits.add(current);
        pointer++;
    }

    public void undo() throws CannotUndoException {
        if (!canUndo()) {
            throw new CannotUndoException();
        }

        RumCompoundEdit u=edits.get(pointer);
        u.undo();
        pointer--;

        refreshControls();
    }

    public void redo() throws CannotUndoException {
        if (!canRedo()) {
            throw new CannotUndoException();
        }

        pointer++;
        RumCompoundEdit u=edits.get(pointer);
        u.redo();

        refreshControls();
    }

    public boolean canUndo() {
        return pointer>=0;
    }

    public boolean canRedo() {
        return edits.size()>0 && pointer<edits.size()-1;
    }

    public void refreshControls() {
//        btnUndo.setEnabled(canUndo());
//        btnRedo.setEnabled(canRedo());
    }
}
