package metro;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;

import lamu.lib.log.Logger;

public class MetroTrackSynchronizerBasic {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }

    public static MetroTrackSynchronizer immediate(long delay) {
        return new MetroTrackSynchronizer() {
            @Override
            public long syncronizeTrack( Metro metro, MetroTrack track, List<MetroTrack> tracks, long measureLengthInFrames) {
                return delay;
            }
        };
    }
    public static final MetroTrackSynchronizer IMMEDIATE = immediate(0);
    public static MetroTrackSynchronizer immediate() {
        return MetroTrackSynchronizerBasic.IMMEDIATE;
    }
    public static MetroTrackSynchronizer parallel( MetroSelector<MetroTrack> trackSelector, double delay ) {
        return new MetroTrackSynchronizer() {
            @Override
            public long syncronizeTrack( Metro metro, MetroTrack track, List<MetroTrack> tracks, long measureLengthInFrames) {
                ArrayList<MetroTrack> selectedTracks = new ArrayList<>();
                trackSelector.selectTracks(tracks, selectedTracks);
                long currentPosition;
                long delayInFrames = (long)(delay * measureLengthInFrames);

                if ( selectedTracks.isEmpty() ) {
                    MetroTrackSynchronizerBasic.logError( "no track was selected (" + trackSelector + ")" , new Exception());
                    currentPosition = 0;
                } else {
                    MetroSequence sequence = selectedTracks.get(0).getSequence();
                    if ( sequence instanceof MetroSynchronizable ) {
                        currentPosition = ((MetroSynchronizable)sequence).getCurrentPositionInFrames(metro);
                    } else {
                        MetroTrackSynchronizerBasic.logError( "the specified track sequence was not synchronizable (" + sequence + ")" , new Exception());
                        currentPosition = 0;
                    }
                }
                return currentPosition + delayInFrames;
            }
        };
    }
    public static MetroTrackSynchronizer serial( MetroSelector<MetroTrack> trackSelector, double delay ) {
        return new MetroTrackSynchronizer() {
            @Override
            public long syncronizeTrack( Metro metro, MetroTrack track, List<MetroTrack> tracks, long measureLengthInFrames) {
                ArrayList<MetroTrack> selectedTracks = new ArrayList<>();
                trackSelector.selectTracks(tracks, selectedTracks);
                long currentPosition;
                long currentLength ;
                long delayInFrames = (long)(delay * measureLengthInFrames);

                if ( selectedTracks.isEmpty() ) {
                    MetroTrackSynchronizerBasic.logError( "no track was selected (" + trackSelector + ")" , new Exception());
                    currentPosition = 0;
                    currentLength  = 0;
                } else {
                    MetroSequence sequence = selectedTracks.get(0).getSequence();
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
        };
    }
    
    
    
    

}
