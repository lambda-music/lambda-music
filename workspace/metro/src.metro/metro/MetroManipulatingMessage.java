package metro;

import java.util.List;

public final class MetroManipulatingMessage implements MetroMessage {
    private final MetroTrackManipulator manipulator;
    public MetroManipulatingMessage( MetroTrackManipulator manipulator ) {
        this.manipulator = manipulator;
    }
    @Override
    public void executeMessage(
        Metro metro,
        List<MetroTrack> currentTracks,
        List<MetroTrack> registeringTracks,
        List<MetroTrack> finalizingTracks,
        List<MetroTrack> unregisteringTracks, 
        long measureLengthInFrames) 
    {
        manipulator.manipulateTracks( currentTracks, registeringTracks, finalizingTracks, unregisteringTracks );
    }
}