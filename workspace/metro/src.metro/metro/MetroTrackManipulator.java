package metro;

import java.util.List;

public interface MetroTrackManipulator {
    void manipulateTracks(
        List<MetroTrack> currentTracks,
        List<MetroTrack> registeringTracks,
        List<MetroTrack> removingTracks,
        List<MetroTrack> unregisteringTracks);
    
    class MetroTrackIdle implements MetroTrackManipulator {
        @Override
        public void manipulateTracks(
            List<MetroTrack> currentTracks,
            List<MetroTrack> registeringTracks,
            List<MetroTrack> removingTracks,
            List<MetroTrack> unregisteringTracks)
        {
        }
        @Override
        public String toString() {
            return MetroTrackManipulator.toStringProc( "idle", "#f" );
        }
    }
    MetroTrackManipulator IDLE = new MetroTrackIdle();

    public static String toStringProc( String type, Object value ) {
        return String.format("(mant type: '%s value: %s)", type, value );
    }
}

