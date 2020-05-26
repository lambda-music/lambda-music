package metro;

import java.lang.invoke.MethodHandles;
import java.util.List;
import java.util.logging.Level;

import lamu.lib.log.Logger;

public class MetroVoidSequence implements MetroSequence {
    private static final MetroVoidSequence INSTANCE = new MetroVoidSequence();
    public static MetroVoidSequence getInstance() {
        return INSTANCE;
    }
    
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }
    
    @Override
    public void progressBuffer(Metro metro, MetroTrack track, long measureLengthInFrames) throws MetroException {
    }
    @Override
    public void progressCursor(
        Metro metro, MetroTrack track, 
        long nframes,
        long measureLengthInFrames, List<MetroMidiEvent> inputMidiEvents, List<MetroMidiEvent> outputMidiEvents, List<MetroTrack> tracks, List<MetroTrack> registeringTracks, List<MetroTrack> finalizingTracks, List<MetroTrack> unregisteringTracks) throws MetroException 
    {
    }
}
