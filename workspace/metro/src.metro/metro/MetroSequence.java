package metro;

import java.util.List;

public interface MetroSequence {
    /**
     * @param metro
     * @param track
     * @param nframes
     * @param measureLengthInFrames
     * @param inputMidiEvents
     * @param outputMidiEvents
     * @param tracks
     * @param registeringTracks
     * @param finalizingTracks 
     * @param unregisteringTracks
     * @throws MetroException
     */
    public abstract void process(Metro metro, MetroTrack track, 
        long nframes, 
        long measureLengthInFrames, 
        List<MetroMidiEvent> inputMidiEvents, 
        List<MetroMidiEvent> outputMidiEvents,
        List<MetroTrack> tracks, 
        List<MetroTrack> registeringTracks, 
        List<MetroTrack> finalizingTracks, 
        List<MetroTrack> unregisteringTracks ) throws MetroException;
}
