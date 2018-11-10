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
import java.util.Set;
 
/**
 * {@link MetroSequence} is a base class of all tracks which are played by the Metro track.
 * 
 * @author ats
 */
public abstract class MetroSequence implements MetroTrackInfo {
	public abstract void    processDirect(    Metro metro, List<MetroAbstractMidiEvent> in, List<MetroAbstractMidiEvent> out );
	public abstract boolean processBuffered(  Metro metro, MetroTrack track, MetroEventBuffer buf );
	
	MetroTrackInfo trackInfo=null;
	public MetroTrackInfo getTrackInfo() {
		return trackInfo;
	}
	public void setTrackInfo(MetroTrackInfo trackInfo) {
		if ( this.trackInfo != null )
			throw new RuntimeException( "the player property is already set" );
		this.trackInfo = trackInfo;
	}
	////////
	public final void check() {
		if ( trackInfo == null )
			throw new RuntimeException( "No ParentPlayer is specified." );
	}
	@Override
	public String getTrackName() {
		check();
		return trackInfo.getTrackName();
	}
	@Override
	public Set<String> getTrackTags() {
		check();
		return trackInfo.getTrackTags();
	}
	@Override
	public boolean isTrackEnabled() {
		check();
		return trackInfo.isTrackEnabled();
	}
	@Override
	public void setTrackEnabled(boolean enabled) {
		check();
		trackInfo.setTrackEnabled(enabled);
	}
	@Override
	public void removeTrack(boolean graceful) {
		check();
		trackInfo.removeTrack( graceful );
	}
	@Override
	public double getTrackPosition() {
		check();
		return trackInfo.getTrackPosition();
	}

	
}
