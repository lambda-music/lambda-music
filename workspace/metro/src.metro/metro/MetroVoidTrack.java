package metro;

import java.lang.invoke.MethodHandles;
import java.util.Collection;
import java.util.List;
import java.util.logging.Level;

import lamu.lib.log.Logger;

public class MetroVoidTrack extends MetroTrack {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }
    
    public MetroVoidTrack(Object name, Collection<Object> tags ) {
        super(name, tags);
    }
    @Override
    public void processBuffer(Metro metro, int barLengthInFrames) throws MetroException {
    }
    @Override
    public void progressCursor(
        Metro metro, int nframes, 
        List<MetroMidiEvent> inputMidiEventList,
        List<MetroMidiEvent> outputMidiEventList) throws MetroException 
    {
    }
}
