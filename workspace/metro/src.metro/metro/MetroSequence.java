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
	 * @return 
	 * the return value is designed to be used as multi-purpose value. MetroSequence does not define
	 * any specific purpose how this value should be interpreted.<p/>
	 * When it is used as MetroTrack, this should always be null; otherwise, the return value
	 * will be ignored.<p/>
	 * When it is used with other objects, it should follow  the specification of the host object. 
	 *  
	 * @throws MetroException
	 */
	public abstract void process(
			Metro metro, 
			MetroTrack track, 
			long nframes, 
			long measureLengthInFrames, 
			List<MetroMidiEvent> inputMidiEvents, 
			List<MetroMidiEvent> outputMidiEvents,
			List<MetroTrack> tracks, 
			List<MetroTrack> registeringTracks, 
			List<MetroTrack> finalizingTracks, 
			List<MetroTrack> unregisteringTracks ) throws MetroException;
}
