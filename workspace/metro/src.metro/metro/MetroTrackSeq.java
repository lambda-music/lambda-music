package metro;

import java.util.List;

public interface MetroTrackSeq {
    // This method was formerly checkBuffer()
    /**
     * 
     * @param metro
     * @param track
     * @param measureLengthInFrames
     * @throws MetroException
     */
    public abstract void progressBuffer(Metro metro, MetroTrack track, long measureLengthInFrames) throws MetroException;
    
    /**
     * 
     * @param metro
     * @param track
     * @param nframes
     * @param measureLengthInFrames
     * @param inputMidiEventList
     * @param outputMidiEventList
     * @param tracks
     * @param registeringTrackList
     * @param unregisteringTrackList
     * @throws MetroException
     */
    public abstract void progressCursor(Metro metro, MetroTrack track, 
        long nframes, 
        long measureLengthInFrames, 
        List<MetroMidiEvent> inputMidiEventList, 
        List<MetroMidiEvent> outputMidiEventList,
        List<MetroTrack> tracks, 
        List<MetroTrack> registeringTrackList, List<MetroTrack> unregisteringTrackList ) throws MetroException;
}
