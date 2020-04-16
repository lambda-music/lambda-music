package metro;

import java.lang.invoke.MethodHandles;
import java.util.List;
import java.util.logging.Level;

import lamu.lib.log.Logger;

public class MetroSequenceDirectThru implements MetroSequence, MetroLock {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }

    public MetroSequenceDirectThru( int delay, MetroPort inputPort, MetroPort outputPort ) {
        this.delay = delay;
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
    private final Object lock = new Object();
    @Override
    public Object getMetroLock() {
        return lock;
    }
    
    int delay = -1;
    public int getRecordLength() {
        return delay;
    }
    public void setRecordLength(int delay) {
        this.delay = delay;
    }

    @Override
    public void processDirect(Metro metro, int nframes, int totalCursor, List<MetroMidiEvent> in, List<MetroMidiEvent> out) {
        out.addAll(in);
        MetroSequenceDirectFilter.bufferReplacePort( in, inputPort , outputPort );

    }

    @Override
    public <T> void processBuffered( Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T> buffer ) {
    }
}
