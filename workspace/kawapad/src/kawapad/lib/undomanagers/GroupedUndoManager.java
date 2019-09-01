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
import java.util.logging.Logger;

import javax.swing.event.UndoableEditListener;
import javax.swing.undo.UndoableEdit;

public abstract interface GroupedUndoManager extends UndoableEdit, UndoableEditListener {
	public static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
	public static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
	public static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
	public static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }
	static final boolean DEBUG = false;
	public abstract void startGroup0();
	public default void startGroup() {
		if ( DEBUG )
			logInfo( "GroupedUndoManager.startGroup() >>>" );
		this.startGroup0();
	}
	public default void endGroup() {
		if ( DEBUG )
			logInfo( "GroupedUndoManager.endGroup() <<<" );
		this.startGroup0();
	}
	public abstract void setSuspended(boolean suspended);
	public abstract boolean isSuspended();
	
	void discardAllEdits();
	
//	public default void dump() {
//		System.err.println( "==== DUMP ====" );
//		
//		for ( int i=0; i<edits.size(); i++  ) {
//			System.err.println( edits.get(i) + ":" + (  indexOfNextAdd == i ? "[CURRENT]" : ""  ) );
//		}
//		System.err.println( "" );
//	}
}
