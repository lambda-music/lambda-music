package metro;

import java.util.Collection;
import java.util.List;

/**
 * Added (Thu, 14 May 2020 06:21:16 +0900)
 * 
 * @author ats
 */
public interface MetroEventOutput {
    /**
     * This method defines the behavior when the current event is in the range where 
     * JACkAudio system is currently playing. The default behavior should be just ignoring the event.
     * Currently only {@link MetroMidiEvent} class defines a meaningful behavior. 
     * See {@link MetroMidiEvent#processOutput(Collection, List, List, List)}.
     * 
     * See also {@link MetroBufferedTrackSeq#searchEventBuffer(Metro, java.util.List, List, List, List, List, long, long)}
     * because it is the only method which calls this method. 
     * 
     * @param output
     * @param tracks TODO
     * @param registeringTrackList TODO
     * @param unregisteringTrackList TODO
     */
    public void processOutput( 
        Collection<MetroMidiEvent> output, 
        List<MetroTrack> tracks, 
        List<MetroTrack> registeringTrackList,
        List<MetroTrack> unregisteringTrackList ); 
}
