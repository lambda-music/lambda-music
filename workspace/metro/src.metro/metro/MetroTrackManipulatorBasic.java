package metro;

import java.util.List;

public class MetroTrackManipulatorBasic {
    public static MetroTrackManipulator remove( MetroTrackSelector selector ) { 
        class MetroTrackRemover implements MetroTrackManipulator {
            private final MetroTrackSelector selector;
            public MetroTrackRemover(MetroTrackSelector selector) {
                this.selector = selector;
            }
            @Override
            public void createTracks(List<MetroTrack> currentTracks, 
                                     List<MetroTrack> registeringTracks,
                                     List<MetroTrack> unregisteringTracks) 
            {
                selector.selectTracks(currentTracks, unregisteringTracks);
            }
        }
        return new MetroTrackRemover(selector);
    }
}

