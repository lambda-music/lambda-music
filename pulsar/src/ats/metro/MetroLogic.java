package ats.metro;

import java.util.List;
import java.util.Set;

public abstract class MetroLogic implements MetroPlayer {
	public abstract void processInputMidiBuffer( Metro metro, List<MetroMidiEvent> in, List<MetroMidiEvent> out );
	public abstract boolean processOutputNoteBuffer( Metro metro, MetroNoteEventBufferSequence sequence, MetroNoteEventBuffer buf );
	
	MetroPlayer player=null;
	public MetroPlayer getPlayer() {
		return player;
	}
	public void setPlayer(MetroPlayer player) {
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
	public void playerRemove() {
		check();
		player.playerRemove();
	}
	@Override
	public double getPosition() {
		check();
		return player.getPosition();
	}

	
}
