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

package metro;

import java.util.List;
 
/**
 * {@link MetroSequence} is a base class of all tracks which are played by the Metro track.
 * 
 * @author ats
 */
public abstract interface MetroSequence {
    public abstract void processDirect(   Metro metro, int nframes, int totalCursor, List<MetroMidiEvent> in, List<MetroMidiEvent> out );
    public abstract <T> void processBuffered( Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T> buffer );
}
