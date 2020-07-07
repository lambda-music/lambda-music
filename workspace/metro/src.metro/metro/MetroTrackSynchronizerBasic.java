package metro;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;

import lamu.lib.logging.Logger;

public class MetroTrackSynchronizerBasic {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }

    /////////////////////////////////////////////////////////////////////////////
    //
    // Synchronizer Factory Map
    //
    /////////////////////////////////////////////////////////////////////////////

    private static final MetroFactoryMap<MetroTrackSynchronizerFactory> factoryMap = new MetroFactoryMap<MetroTrackSynchronizerFactory>();
    public static MetroFactoryMap<MetroTrackSynchronizerFactory> getFactoryMap() {
        return factoryMap;
    }
    
    /////////////////////////////////////////////////////////////////////////////
    //
    // Long Immediate Synchronizer 
    // The offset value should be specified by a number which is measured by frame-count.
    //
    /////////////////////////////////////////////////////////////////////////////

    static final class LongImmediateTrackSynchronizer implements MetroTrackSynchronizer {
        private final long syncOffset;
        public LongImmediateTrackSynchronizer(long syncOffset) {
            this.syncOffset = syncOffset;
        }
        @Override
        public long syncronizeTrack( Metro metro, MetroTrack track, List<MetroTrack> tracks, long measureLengthInFrames) {
            return syncOffset;
        }
    }
    public static MetroTrackSynchronizer immediate( long syncOffset ) {
        return new LongImmediateTrackSynchronizer(syncOffset);
    }

    /////////////////////////////////////////////////////////////////////////////
    //
    // Double Immediate Synchronizer 
    //
    /////////////////////////////////////////////////////////////////////////////
    
    static final class DoubleImmediateTrackSynchronizer implements MetroTrackSynchronizer {
        private final double syncOffset;
        public DoubleImmediateTrackSynchronizer( double syncOffset) {
            this.syncOffset = syncOffset;
        }
        @Override
        public long syncronizeTrack( Metro metro, MetroTrack track, List<MetroTrack> tracks, long measureLengthInFrames) {
            return (long) (syncOffset*measureLengthInFrames);
        }
    }
    static final MetroTrackSynchronizer IMMEDIATE = immediate(0);
    public static MetroTrackSynchronizer immediate() {
        return MetroTrackSynchronizerBasic.IMMEDIATE;
    }
    public static MetroTrackSynchronizer immediate(double syncOffset) {
        return new DoubleImmediateTrackSynchronizer(syncOffset);
    }
    static final class ImmediateSynchronizerFactory implements MetroTrackSynchronizerFactory {
        @Override
        public MetroTrackSynchronizer createSynchronizer(MetroTrackSelector syncTrack, double syncOffset) {
            return immediate(syncOffset);
        }
    }
    static {
        MetroTrackSynchronizerFactory factory = new ImmediateSynchronizerFactory();
        getFactoryMap().addFactory("immediate", factory);
        getFactoryMap().addFactory("imme", factory);
        getFactoryMap().addFactory("i", factory);
    }

    /////////////////////////////////////////////////////////////////////////////
    //
    // Basic Synchronizer 
    //
    /////////////////////////////////////////////////////////////////////////////

    /**
     * This class is an utility base class to implement
     * {@link MetroTrackSynchronizer}. This class is only for the track
     * synchronizers and subjected to be local to this class. This class. retrieves
     * the tracks which are selected by the given track selectors and retrieves four
     * values which are necessary to calculate the offset, and then call the
     * {@link #calculateOffset(long, long, long, long, long)} method to calculate
     * the value.
     */
    static abstract class BasicTrackSynchronizer implements MetroTrackSynchronizer {
        private final MetroTrackSelector syncTrack;
        private final double             syncOffset;

        public BasicTrackSynchronizer(MetroTrackSelector syncTrack, double syncOffset) {
            this.syncTrack  = syncTrack;
            this.syncOffset = syncOffset;
        }

        @Override
        public long syncronizeTrack( Metro metro, MetroTrack track, List<MetroTrack> tracks, long measureLengthInFrames) {
            // 1.
            long delayInFrames = (long)(syncOffset * measureLengthInFrames);

            // 2. Get the current position/length of the current track. 
            long positionInFrames;
            long lengthInFrames;
            
            {
                MetroSequence sequence = track.getSequence();
                if ( sequence instanceof MetroSynchronizable ) {
                    MetroSynchronizable syncSequence = (MetroSynchronizable)sequence;
                    positionInFrames = syncSequence.getCurrentPositionInFrames(metro);
                    lengthInFrames   = syncSequence.getCurrentLengthInFrames(metro);
                } else {
                    MetroTrackSynchronizerBasic.logError( "the current track sequence was not synchronizable (" + sequence + ")" , new Exception());
                    positionInFrames = 0;
                    lengthInFrames   = measureLengthInFrames;
                }
            }

            // 3. Invoke the track selector and select tracks.
            ArrayList<MetroTrack> selectedTracks = new ArrayList<>();
            syncTrack.selectTracks(tracks, selectedTracks);


            // 4. Get the current position/length of the synchronizing target track. 
            long syncPositionInFrames;
            long syncLengthInFrames;

            if ( selectedTracks.isEmpty() ) {
                MetroTrackSynchronizerBasic.logWarn( "=== no track was selected ===" );
                MetroTrackSynchronizerBasic.logWarn( "track selector (" + syncTrack + ")" );
                MetroTrackSynchronizerBasic.logWarn( "current tracks (" + tracks + ")" );
                MetroTrackSynchronizerBasic.logError( "" , new Error() );
                syncPositionInFrames = 0;
                syncLengthInFrames   = measureLengthInFrames;
            } else {
                MetroSequence sequence = selectedTracks.get(0).getSequence();
                MetroTrackSynchronizerBasic.logWarn( "track selector selected track (" + syncTrack + ")" );
                if ( sequence instanceof MetroSynchronizable ) {
                    MetroSynchronizable syncSequence = (MetroSynchronizable)sequence;
                    syncPositionInFrames = syncSequence.getCurrentPositionInFrames(metro);
                    syncLengthInFrames   = syncSequence.getCurrentLengthInFrames(metro);
                } else {
                    MetroTrackSynchronizerBasic.logError( "the specified track sequence was not synchronizable (" + sequence + ")" , new Exception());
                    syncPositionInFrames = 0;
                    syncLengthInFrames   = measureLengthInFrames;
                }
            }
            long result = calculateOffset( positionInFrames, lengthInFrames, syncPositionInFrames, syncLengthInFrames, delayInFrames );
            MetroTrackSynchronizerBasic.logInfo(
                String.format( "synchronizer(basic)%s: curr %6d/%6d  sync %6d/%6d offset %6d => %6d ",
                    track.getName(),
                    positionInFrames,
                    lengthInFrames,
                    syncPositionInFrames,
                    syncLengthInFrames,
                    delayInFrames,
                    result ));
            
            return result;
        }

        public abstract long calculateOffset(long positionInFrames, long lengthInFrames, long syncPositionInFrames, long syncLengthInFrames, long delayInFrames);
    }


    /////////////////////////////////////////////////////////////////////////////
    //
    // Parallel Head Synchronizer 
    //
    /////////////////////////////////////////////////////////////////////////////

    static final class ParallelHeadTrackSynchronizer extends BasicTrackSynchronizer {
        public ParallelHeadTrackSynchronizer(MetroTrackSelector syncTrack, double syncOffset) {
            super(syncTrack, syncOffset);
        }
        @Override
        public long calculateOffset(
            long positionInFrames,
            long lengthInFrames,
            long syncPositionInFrames,
            long syncLengthInFrames,
            long delayInFrames) 
        {
            return syncPositionInFrames + delayInFrames;
        }

    }
    public static ParallelHeadTrackSynchronizer parallelHead(MetroTrackSelector syncTrack, double syncOffset) {
        return new ParallelHeadTrackSynchronizer(syncTrack, syncOffset);
    }
    static final class ParallelHeadSynchronizerFactory implements MetroTrackSynchronizerFactory {
        @Override
        public MetroTrackSynchronizer createSynchronizer(MetroTrackSelector syncTrack, double syncOffset) {
            return parallelHead(syncTrack, syncOffset);
        }
    }
    static {
        MetroTrackSynchronizerFactory factory = new ParallelHeadSynchronizerFactory();
        getFactoryMap().addFactory("parallel", factory);
        getFactoryMap().addFactory("para", factory);
        getFactoryMap().addFactory("p", factory);

        getFactoryMap().addFactory("join", factory);
        getFactoryMap().addFactory("j", factory);
    } 

    /////////////////////////////////////////////////////////////////////////////
    //
    // Parallel Tail Synchronizer 
    //
    /////////////////////////////////////////////////////////////////////////////
    static final class ParallelTailTrackSynchronizer extends BasicTrackSynchronizer {
        public ParallelTailTrackSynchronizer(MetroTrackSelector syncTrack, double syncOffset) {
            super(syncTrack, syncOffset);
        }
        @Override
        public long calculateOffset(
            long positionInFrames,
            long lengthInFrames,
            long syncPositionInFrames,
            long syncLengthInFrames,
            long delayInFrames) 
        {
            logInfo( "lengthInFrames:" + lengthInFrames );
            logInfo( "( syncLengthInFrames - syncPositionInFrames ):" + ( syncLengthInFrames - syncPositionInFrames ) );
            return
                lengthInFrames - ( syncLengthInFrames - syncPositionInFrames ) 
                 + delayInFrames;
        }
    }

    public static MetroTrackSynchronizer parallelBottom(MetroTrackSelector syncTrack, double syncOffset) {
        return new ParallelTailTrackSynchronizer(syncTrack, syncOffset);
    }
    static final class ParallelBottomTrackSynchronizerFactory implements MetroTrackSynchronizerFactory {
        @Override
        public MetroTrackSynchronizer createSynchronizer(MetroTrackSelector syncTrack, double syncOffset) {
            return parallelBottom(syncTrack, syncOffset);
        }
    }
    static {
        MetroTrackSynchronizerFactory factory = new ParallelBottomTrackSynchronizerFactory();
        getFactoryMap().addFactory("tail", factory);
        getFactoryMap().addFactory("t", factory);
    } 

    
    /////////////////////////////////////////////////////////////////////////////
    //
    // Serial Synchronizer 
    //
    /////////////////////////////////////////////////////////////////////////////

    static final class SerialTrackSynchronizer implements MetroTrackSynchronizer {
        private final MetroTrackSelector syncTrack;
        private final double             syncOffset;
        public SerialTrackSynchronizer(MetroTrackSelector syncTrack, double syncOffset) {
            this.syncOffset = syncOffset;
            this.syncTrack  = syncTrack;
        }
        
        @Override
        public long syncronizeTrack( Metro metro, MetroTrack track, List<MetroTrack> tracks, long measureLengthInFrames ) {
            ArrayList<MetroTrack> selectedTracks = new ArrayList<>();
            syncTrack.selectTracks(tracks, selectedTracks);
            long currentPosition;
            long currentLength ;
            long delayInFrames = (long)(syncOffset * measureLengthInFrames);
            
            if ( selectedTracks.isEmpty() ) {
                MetroTrackSynchronizerBasic.logError( "no track was selected (" + syncTrack + ")" , new Exception());
                currentPosition = 0;
                currentLength  = 0;
            } else {
                MetroSequence sequence = selectedTracks.get(0).getSequence();
                MetroTrackSynchronizerBasic.logWarn( "track selector selected track=(" + syncTrack + ")" );
                if ( sequence instanceof MetroSynchronizable ) {
                    MetroSynchronizable syncSeq = (MetroSynchronizable)sequence;
                    currentLength = syncSeq.getCurrentLengthInFrames(metro);
                    currentPosition = syncSeq.getCurrentPositionInFrames(metro);
                } else {
                    MetroTrackSynchronizerBasic.logError( "the specified track sequence was not synchronizable (" + sequence + ")" , new Exception());
                    currentLength = 0;
                    currentPosition = 0;
                }
            }
            return currentPosition - currentLength + delayInFrames;
        }
    }
    public static MetroTrackSynchronizer serial( MetroTrackSelector syncTrack, double syncOffset ) {
        return new SerialTrackSynchronizer( syncTrack, syncOffset );
    }
    static final class SerialSynchronizerFactory implements MetroTrackSynchronizerFactory {
        @Override
        public MetroTrackSynchronizer createSynchronizer(MetroTrackSelector syncTrack, double syncOffset) {
            return serial(syncTrack, syncOffset );
        }
    }

    static {
        MetroTrackSynchronizerFactory factory = new SerialSynchronizerFactory();
        getFactoryMap().addFactory("serial", factory);
        getFactoryMap().addFactory("seri", factory);
        getFactoryMap().addFactory("s", factory);

        getFactoryMap().addFactory("head", factory);
        getFactoryMap().addFactory("h", factory);

    }
}
