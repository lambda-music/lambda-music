package ats.metro;

import java.util.Set;

public interface MetroTrackInfo {
	public String getTrackName();
	public Set<String> getTrackTags();
	public boolean isTrackEnabled();
	public void setTrackEnabled(boolean enabled);
	public void removeTrack(boolean graceful);
	public double getTrackPosition();
}
