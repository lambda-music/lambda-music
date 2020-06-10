package metro;

import java.util.Arrays;
import java.util.List;

public interface MetroPutt extends MetroMant {
    public default void putTrack( MetroTrack track )  {
        putTracks( Arrays.asList(track), null );
    }
    public default void putTracks( List<MetroTrack> tracks )  {
        putTracks( tracks, null );
    }
    public default void putTracks( List<MetroTrack> tracks, MetroTrackSynchronizer trackSynchronizer )  {
        manipulateTrack( Arrays.asList( MetroTrackManipulatorBasic.replace(tracks, trackSynchronizer))); 
    }
}
