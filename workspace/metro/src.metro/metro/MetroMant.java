package metro;

import java.util.List;

public interface MetroMant extends MetroVerb {
    void manipulateTrack( List<MetroTrackManipulator> manipulators );
}
