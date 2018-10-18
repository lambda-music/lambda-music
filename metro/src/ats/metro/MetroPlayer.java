package ats.metro;

import java.util.Set;

public interface MetroPlayer {
	public String getPlayerName();
	public Set<String> getPlayerTags();
	public boolean isPlayerEnabled();
	public void setPlayerEnabled(boolean enabled);
	public void playerRemove(boolean graceful);
	public double getPosition();
}
