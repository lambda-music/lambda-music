package ats.metro;

import java.util.List;
import java.util.Set;
 
/**
 * {@link MetroLogic} is a base class of all tracks which are played by the Metro track.
 * 
 * @author ats
 */
public abstract class MetroLogic implements MetroPlayer {
	public abstract void    processDirect(    Metro metro, List<MetroAbstractMidiEvent> in, List<MetroAbstractMidiEvent> out );
	public abstract boolean processBuffered(  Metro metro, MetroTrack track, MetroEventBuffer buf );
	
	MetroPlayer player=null;
	public MetroPlayer getPlayer() {
		return player;
	}
	public void setPlayer(MetroPlayer player) {
		if ( this.player != null )
			throw new RuntimeException( "the player property is already set" );
		this.player = player;
	}
	////////
	public final void check() {
		if ( player == null )
			throw new RuntimeException( "No ParentPlayer is specified." );
	}
	@Override
	public String getPlayerName() {
		check();
		return player.getPlayerName();
	}
	@Override
	public Set<String> getPlayerTags() {
		check();
		return player.getPlayerTags();
	}
	@Override
	public boolean isPlayerEnabled() {
		check();
		return player.isPlayerEnabled();
	}
	@Override
	public void setPlayerEnabled(boolean enabled) {
		check();
		player.setPlayerEnabled(enabled);
	}
	@Override
	public void playerRemove(boolean graceful) {
		check();
		player.playerRemove( graceful );
	}
	@Override
	public double getPosition() {
		check();
		return player.getPosition();
	}

	
}
