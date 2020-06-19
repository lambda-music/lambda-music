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

import lamu.lib.log.Logger;


public class EmptyGroupedUndoManager extends UndoManager implements GroupedUndoManager {
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
    
    public synchronized void undoableEditHappened(UndoableEditEvent e) {
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
}
