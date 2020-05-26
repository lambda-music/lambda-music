package metro;

import java.util.Arrays;
import java.util.List;

public interface MetroPutt extends MetroMant {
    public default void putTrack( MetroTrack track )  {
        putTrack( Arrays.asList(track), null );
    }
    public default void putTrack( List<MetroTrack> trackList )  {
        putTrack( trackList, null );
    }
    public default void putTrack( List<MetroTrack> tracks, MetroTrackSynchronizer trackSynchronizer )  {
        manipulateTrack(
            MetroTrackManipulatorBasic.synchronizedStopper(
                MetroTrackManipulatorBasic.removing( 
                    MetroTrackSelectorBasic.correspondingNamedTrack(tracks)),
                trackSynchronizer));
        
        manipulateTrack( 
            MetroTrackManipulatorBasic.synchronizedStarter(
                MetroTrackManipulatorBasic.registering(
                    MetroTrackSelectorBasic.constant(tracks)),
                trackSynchronizer));
    }
}
