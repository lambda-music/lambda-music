/*
 * Metro Musical Sequencing Framework written by Atsushi Oka 
 * Copyright 2018 Atsushi Oka
 *
 * This file is part of Metro Musical Sequencing Framework. 
 * 
 * Metro Musical Sequencing Framework is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * Metro Musical Sequencing Framework is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with Metro Musical Sequencing Framework.  If not, see <https://www.gnu.org/licenses/>.
 */

package ats.metro;

import java.util.List;

public class MetroMessageEvent extends MetroEvent {
	private final Runnable message;
	public MetroMessageEvent(double offset, Runnable message ) {
		super(offset);
		this.message = message;
	}
	public void execute( Metro metro ) {
		metro.postMessage( message );
	}
	@Override
	public void process(Metro metro, int from, int to, int nframes, List<MetroAbstractMidiEvent> eventList) {
		execute( metro );		
	}
}