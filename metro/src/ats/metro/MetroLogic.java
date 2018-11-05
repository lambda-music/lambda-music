package ats.metro;

import java.util.List;
import java.util.Set;
 
/**
 * {@link MetroLogic} is a base class of all tracks which are played by the Metro track.
 * 
 * @author ats
 */
public abstract class MetroLogic implements MetroTrackInfo {
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
