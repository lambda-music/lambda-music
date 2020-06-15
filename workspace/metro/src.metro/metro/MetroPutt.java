package metro;

import java.util.Arrays;
import java.util.List;

public interface MetroPutt extends MetroMant {
    public default void putTrack( MetroTrack track )  {
        putTracks( Arrays.asList(track), null, null);
    }
    public default void putTracks( List<MetroTrack> tracks )  {
        putTracks( tracks, null, null );
    }
    public default void putTracks( List<MetroTrack> tracks, MetroTrackSynchronizer startSynchronizer, MetroTrackSynchronizer stopSynchronizer )  {
        manipulateTrack( Arrays.asList( MetroTrackManipulatorBasic.replace( tracks, startSynchronizer,stopSynchronizer ))); 
    }
}
