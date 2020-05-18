package metro;

import java.lang.invoke.MethodHandles;
import java.util.List;
import java.util.logging.Level;

import lamu.lib.log.Logger;

public class MetroThruTrackSeq implements MetroTrackSeq {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }

    
    public MetroThruTrackSeq(MetroPort inputPort, MetroPort outputPort ) {
        this.inputPort = inputPort;
        this.outputPort = outputPort;
    }
    private MetroPort inputPort;
    private MetroPort outputPort;
    public MetroPort getInputPort() {
        return inputPort;
    }
    public MetroPort getOutputPort() {
        return outputPort;
    }
    @Override
    public void progressBuffer(Metro metro, MetroTrack track, long measureLengthInFrames) throws MetroException {
    }
    @Override
    public void progressCursor(
        Metro metro, MetroTrack track, 
        long nframes,
        long measureLengthInFrames, List<MetroMidiEvent> inputMidiEventList, List<MetroMidiEvent> outputMidiEventList, List<MetroTrack> tracks, List<MetroTrack> registeringTrackList, List<MetroTrack> unregisteringTrackList) throws MetroException 
    {
        MetroDirectFilterTrackSeq.bufferReplacePort( inputMidiEventList, inputPort , outputPort );
        outputMidiEventList.addAll(inputMidiEventList);
    }
}
