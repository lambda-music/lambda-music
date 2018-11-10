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

package ats.kawapad.lib;

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
