package metro;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.logging.Level;

import lamu.lib.log.Logger;

public class MetroTrackSynchronizerBasic {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }

    private static HashMap<String,MetroTrackSynchronizerFactory> factoryMap = new HashMap<>();
    public static MetroTrackSynchronizerFactory getFactory( String name ) {
        if ( ! factoryMap.containsKey(name))
            throw new IllegalArgumentException("unknown factory name (" + name + ")");
        return factoryMap.get(name);
    }
    public static void addFactory( String name, MetroTrackSynchronizerFactory factory ) {
        if ( factoryMap.containsKey(name))
            throw new IllegalArgumentException("duplicate factory name (" + name + ")");
        factoryMap.put( name, factory );
    }

    static void argumentCheck( Object[] args, int argumentCount ) {
        if ( args == null )
            throw new NullPointerException( "the argument array is null" );
        if ( args.length != argumentCount )
            throw new IllegalArgumentException( "the argument number("+args.length+") != " + argumentCount );

    }
    
    static final class LongTrackSynchronizer implements MetroTrackSynchronizer {
        private final long syncOffset;
        public LongTrackSynchronizer(long syncOffset) {
            this.syncOffset = syncOffset;
        }
        @Override
        public long syncronizeTrack( Metro metro, MetroTrack track, List<MetroTrack> tracks, long measureLengthInFrames) {
            return syncOffset;
        }
    }
    public static MetroTrackSynchronizer immediate(long syncOffset) {
        return new LongTrackSynchronizer(syncOffset);
    }
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
    public static MetroTrackSynchronizer immediate(double syncOffset) {
        return new DoubleImmediateTrackSynchronizer(syncOffset);
    }
    static final MetroTrackSynchronizer IMMEDIATE = immediate(0);
    public static MetroTrackSynchronizer immediate() {
        return MetroTrackSynchronizerBasic.IMMEDIATE;
    }
    static {
        final class ImmediateFactory implements MetroTrackSynchronizerFactory {
            @Override
            public MetroTrackSynchronizer createSynchronizer(MetroTrackSelector syncTrack, double syncOffset) {
                return immediate(syncOffset);
            }
        }
        MetroTrackSynchronizerFactory factory = new ImmediateFactory();
        addFactory( "immediate" , factory );
        addFactory( "imme" , factory );
        addFactory( "i" , factory );
    }
    

    static final class ParallelTrackSynchronizer implements MetroTrackSynchronizer {
        private final MetroTrackSelector syncTrack;
        private final double             syncOffset;

        public ParallelTrackSynchronizer(MetroTrackSelector syncTrack, double syncOffset) {
            this.syncTrack  = syncTrack;
            this.syncOffset = syncOffset;
        }

        @Override
        public long syncronizeTrack( Metro metro, MetroTrack track, List<MetroTrack> tracks, long measureLengthInFrames) {
            ArrayList<MetroTrack> selectedTracks = new ArrayList<>();
            syncTrack.selectTracks(tracks, selectedTracks);
            long currentPosition;
            long delayInFrames = (long)(syncOffset * measureLengthInFrames);

            if ( selectedTracks.isEmpty() ) {
                MetroTrackSynchronizerBasic.logWarn( "=== no track was selected ===" );
                MetroTrackSynchronizerBasic.logWarn( "track selector (" + syncTrack + ")" );
                MetroTrackSynchronizerBasic.logWarn( "current tracks (" + tracks + ")" );
                MetroTrackSynchronizerBasic.logError( "" , new Error() );
                currentPosition = 0;
            } else {
                MetroSequence sequence = selectedTracks.get(0).getSequence();
                MetroTrackSynchronizerBasic.logWarn( "track selector selected track (" + syncTrack + ")" );
                if ( sequence instanceof MetroSynchronizable ) {
                    currentPosition = ((MetroSynchronizable)sequence).getCurrentPositionInFrames(metro);
                } else {
                    MetroTrackSynchronizerBasic.logError( "the specified track sequence was not synchronizable (" + sequence + ")" , new Exception());
                    currentPosition = 0;
                }
            }
            MetroTrackSynchronizerBasic.logInfo( "synchronizer(parallel):" + track.getName() + ":" + currentPosition  + "/" + delayInFrames );
            return currentPosition + delayInFrames;
        }
    }

    /**
     * 
     * @param syncTrack
     * @param syncOffset
     * @return
     */
    public static MetroTrackSynchronizer parallel( MetroTrackSelector syncTrack, double syncOffset ) {
        return new ParallelTrackSynchronizer(syncTrack, syncOffset);
    }

    static {
        final class ParallelFactory implements MetroTrackSynchronizerFactory {
            @Override
            public MetroTrackSynchronizer createSynchronizer(MetroTrackSelector syncTrack, double syncOffset) {
                return parallel(syncTrack, syncOffset );
            }
        }
        MetroTrackSynchronizerFactory factory = new ParallelFactory();
        addFactory( "parallel" , factory );
        addFactory( "para" , factory );
        addFactory( "p" , factory );
    } 

    
    public static final class SerialTrackSynchronizer implements MetroTrackSynchronizer {
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
    static {
        final class SerialFactory implements MetroTrackSynchronizerFactory {
            @Override
            public MetroTrackSynchronizer createSynchronizer(MetroTrackSelector syncTrack, double syncOffset) {
                return serial(syncTrack, syncOffset );
            }
        }
        MetroTrackSynchronizerFactory factory = new SerialFactory();
        addFactory( "serial" , factory );
        addFactory( "seri" , factory );
        addFactory( "s" , factory );
    }
    
    
    
    

}
