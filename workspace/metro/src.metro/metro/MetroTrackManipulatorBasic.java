package metro;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.logging.Level;

import lamu.lib.log.Logger;

public class MetroTrackManipulatorBasic {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }

    private static final MetroFactoryMap<MetroTrackManipulatorFactory> factoryMap = new MetroFactoryMap<MetroTrackManipulatorFactory>();
    public static MetroFactoryMap<MetroTrackManipulatorFactory> getFactoryMap() {
        return factoryMap;
    }

    public static MetroTrackManipulator idle() {
        return MetroTrackManipulator.IDLE;
    }
    
    static {
        final class IdleFactory implements MetroTrackManipulatorFactory {
            @Override
            public MetroTrackManipulator create(MetroTrackSelector trackSelector, MetroTrackSynchronizer trackSynchronizer) {
                return MetroTrackManipulator.IDLE; 
            }
        }
        MetroTrackManipulatorFactory factory = new IdleFactory();
        getFactoryMap().addFactory( "idle" , factory );
        getFactoryMap().addFactory( "i" , factory );
    }


    public static MetroTrackManipulator removing(MetroTrackSelector trackSelector) {
        class MetroTrackRemover implements MetroTrackManipulator {
            private final MetroTrackSelector selector;

            public MetroTrackRemover(MetroTrackSelector selector) {
                this.selector = selector;
            }
            @Override
            public String toString() {
                return MetroTrackManipulator.toStringProc( "remt", selector );
            }

            @Override
            public void manipulateTracks(
                List<MetroTrack> currentTracks,
                List<MetroTrack> registeringTracks,
                List<MetroTrack> removingTracks,
                List<MetroTrack> unregisteringTracks)
            {
                selector.selectTracks(currentTracks, removingTracks);
            }
        }
        return new MetroTrackRemover(trackSelector);
    }
    public static MetroTrackManipulator removing( MetroTrackSelector trackSelector, MetroTrackSynchronizer trackSynchronizer ) {
        MetroTrackManipulator trackManipulator = removing( trackSelector );
        if ( trackSynchronizer != null )
            trackManipulator = synchronizedStopper(trackManipulator, trackSynchronizer);
        return trackManipulator;
    }

    static {
        final class RemovingFactory implements MetroTrackManipulatorFactory {
            @Override
            public MetroTrackManipulator create(MetroTrackSelector trackSelector, MetroTrackSynchronizer trackSynchronizer) {
                return removing( trackSelector, trackSynchronizer );
            }
        }
        MetroTrackManipulatorFactory factory = new RemovingFactory();
        getFactoryMap().addFactory( "remove-tracks", factory );
        getFactoryMap().addFactory( "remove" , factory );
        getFactoryMap().addFactory( "remt" , factory );
        getFactoryMap().addFactory( "r" , factory );
    }

    
    public static MetroTrackManipulator registering(MetroTrackSelector selector) {
        class MetroTrackRegisterer implements MetroTrackManipulator {
            private final MetroTrackSelector selector;

            public MetroTrackRegisterer(MetroTrackSelector selector) {
                this.selector = selector;
            }
            @Override
            public void manipulateTracks(
                List<MetroTrack> currentTracks,
                List<MetroTrack> registeringTracks,
                List<MetroTrack> removingTracks,
                List<MetroTrack> unregisteringTracks)
            {
                selector.selectTracks(currentTracks, registeringTracks);
            }
            @Override
            public String toString() {
                return String.format("(mant type: %s value: %s)", "putt", selector );
            }
        }
        return new MetroTrackRegisterer(selector);
    }
    
    static {
        final class RegisteringFactory implements MetroTrackManipulatorFactory {
            @Override
            public MetroTrackManipulator create(MetroTrackSelector trackSelector, MetroTrackSynchronizer trackSynchronizer) {
                MetroTrackManipulator trackManipulator = registering( trackSelector );
                if ( trackSynchronizer != null )
                    trackManipulator = synchronizedStarter( trackManipulator, trackSynchronizer );
                return trackManipulator;
            }
        }
        MetroTrackManipulatorFactory factory = new RegisteringFactory();
        getFactoryMap().addFactory( "put-tracks", factory );
        getFactoryMap().addFactory( "put" , factory );
        getFactoryMap().addFactory( "putt" , factory );
        getFactoryMap().addFactory( "p" , factory );
    }


    public static MetroTrackManipulator unregistering(MetroTrackSelector selector) {
        class MetroTrackUnregisterer implements MetroTrackManipulator {
            private final MetroTrackSelector selector;

            public MetroTrackUnregisterer(MetroTrackSelector selector) {
                this.selector = selector;
            }
            @Override
            public String toString() {
                return MetroTrackManipulator.toStringProc( "kilt", selector );
            }

            @Override
            public void manipulateTracks(
                List<MetroTrack> currentTracks,
                List<MetroTrack> registeringTracks,
                List<MetroTrack> removingTracks,
                List<MetroTrack> unregisteringTracks)
            {
                selector.selectTracks(currentTracks, unregisteringTracks);
            }
        }
        return new MetroTrackUnregisterer(selector);
    }

    static {
        final class UnregisteringFactory implements MetroTrackManipulatorFactory {
            @Override
            public MetroTrackManipulator create(MetroTrackSelector trackSelector, MetroTrackSynchronizer trackSynchronizer) {
                MetroTrackManipulator trackManipulator = unregistering( trackSelector );
                if ( trackSynchronizer != null )
                    trackManipulator = synchronizedStopper(trackManipulator, trackSynchronizer);
                return trackManipulator;
            }
        }
        MetroTrackManipulatorFactory factory = new UnregisteringFactory();
        getFactoryMap().addFactory( "kill-tracks", factory );
        getFactoryMap().addFactory( "kill" , factory );
        getFactoryMap().addFactory( "kilt" , factory );
        getFactoryMap().addFactory( "k" , factory );
    }

    
    private static class MetroTrackSynchronizedStarter implements MetroTrackManipulator {
        private final MetroTrackManipulator  manipulator;
        private final MetroTrackSynchronizer trackSynchronizer;

        public MetroTrackSynchronizedStarter(MetroTrackManipulator manipulator,
            MetroTrackSynchronizer trackSynchronizer) {
            this.manipulator       = manipulator;
            this.trackSynchronizer = trackSynchronizer;
        }
        @Override
        public String toString() {
            return MetroTrackManipulator.toStringProc( "starter", manipulator );
        }

        static void proc(MetroTrackSynchronizer trackSynchronizer, MetroTrack track) {
            MetroSequence sequence = track.getSequence();
            if (sequence instanceof MetroSynchronizedStarter) {
                ((MetroSynchronizedStarter) sequence).setStartSynchronizer(trackSynchronizer);
            }
        }

        @Override
        public void manipulateTracks(
            List<MetroTrack> currentTracks,
            List<MetroTrack> registeringTracks,
            List<MetroTrack> removingTracks,
            List<MetroTrack> unregisteringTracks)
        {
            manipulator.manipulateTracks(currentTracks, registeringTracks, removingTracks, unregisteringTracks);

            // Set the track synchronizer to the tracks.
            for (MetroTrack track : registeringTracks) {
                proc(trackSynchronizer, track);
            }
        }
    }

    public static MetroTrackManipulator synchronizedStarter(
        MetroTrackManipulator manipulator,
        MetroTrackSynchronizer trackSynchronizer) 
    {
        if (trackSynchronizer == null)
            return manipulator;
        else
            return new MetroTrackSynchronizedStarter(manipulator, trackSynchronizer);
    }

    static class MetroTrackSynchronizedStopper implements MetroTrackManipulator {
        private final MetroTrackManipulator  manipulator;
        private final MetroTrackSynchronizer trackSynchronizer;

        public MetroTrackSynchronizedStopper(MetroTrackManipulator manipulator,
            MetroTrackSynchronizer trackSynchronizer) {
            super();
            this.manipulator       = manipulator;
            this.trackSynchronizer = trackSynchronizer;
        }
        @Override
        public String toString() {
            return MetroTrackManipulator.toStringProc( "stopper", manipulator );
        }

        static void proc(MetroTrackSynchronizer trackSynchronizer, MetroTrack track) {
            MetroSequence sequence = track.getSequence();
            if (sequence instanceof MetroSynchronizedStopper) {
                ((MetroSynchronizedStopper) sequence).setStopSynchronizer(trackSynchronizer);
            }
        }

        @Override
        public void manipulateTracks(
            List<MetroTrack> currentTracks,
            List<MetroTrack> registeringTracks,
            List<MetroTrack> removingTracks,
            List<MetroTrack> unregisteringTracks)
        {
            manipulator.manipulateTracks(currentTracks, registeringTracks, removingTracks, unregisteringTracks);

            // Set the track synchronizer to the tracks.
            for (MetroTrack track : registeringTracks) {
                proc(trackSynchronizer, track);
            }
        }
    }

    public static MetroTrackManipulator synchronizedStopper( MetroTrackManipulator manipulator, MetroTrackSynchronizer trackSynchronizer) {
        if (trackSynchronizer == null)
            return manipulator;
        else
            return new MetroTrackSynchronizedStopper(manipulator, trackSynchronizer);
    }
    
    static class MetroTrackManipulatorMultiple implements MetroTrackManipulator {
        private final List<MetroTrackManipulator> manipulators;
        public MetroTrackManipulatorMultiple(MetroTrackManipulator ... manipulators  ) {
            super();
            this.manipulators = Arrays.asList( manipulators );
        }
        public MetroTrackManipulatorMultiple(List<MetroTrackManipulator> manipulators ) {
            super();
            this.manipulators = manipulators;
        }
        @Override
        public String toString() {
            return MetroTrackManipulator.toStringProc( "multi", MetroUtils.listToString(Arrays.asList( manipulators )));
        }
        @Override
        public void manipulateTracks(
            List<MetroTrack> currentTracks,
            List<MetroTrack> registeringTracks,
            List<MetroTrack> removingTracks,
            List<MetroTrack> unregisteringTracks) 
        {
            for ( MetroTrackManipulator manipulator : manipulators ) {
                try {
                    manipulator.manipulateTracks(currentTracks, registeringTracks, removingTracks, unregisteringTracks);
                } catch ( Throwable t ) {
                    logError( "", t );
                }
            }
        }
    }
    
    public static MetroTrackManipulator multiple( MetroTrackManipulator ... args )  {
        return new MetroTrackManipulatorMultiple( args );
    }
    public static MetroTrackManipulator multiple( List<MetroTrackManipulator>  argList )  {
        return new MetroTrackManipulatorMultiple( argList );
    }

    
    public static MetroTrackManipulator replace( MetroTrackSelector trackSelector, MetroTrackSynchronizer trackSynchronizer )  {
        ArrayList<MetroTrack> selectedTracks = new ArrayList<>();
        MetroTrackSelector.resolveSelector( Arrays.asList( trackSelector ) ,selectedTracks );
        return replace( selectedTracks, trackSynchronizer );
    }

    public static MetroTrackManipulator replace( List<MetroTrack> tracks, MetroTrackSynchronizer trackSynchronizer )  {
        return multiple(
                MetroTrackManipulatorBasic.synchronizedStopper(
                    MetroTrackManipulatorBasic.removing( 
                        MetroTrackSelectorBasic.correspondingNamedTrack(tracks)),
                    trackSynchronizer),
                MetroTrackManipulatorBasic.synchronizedStarter(
                    MetroTrackManipulatorBasic.registering(
                        MetroTrackSelectorBasic.trackConstant(tracks)),
                    trackSynchronizer));
    }

    static {
        final class ReplaceFactory implements MetroTrackManipulatorFactory {
            @Override
            public MetroTrackManipulator create(
                MetroTrackSelector trackSelector,
                MetroTrackSynchronizer trackSynchronizer) 
            {
                return replace( trackSelector, trackSynchronizer );
            }
        }
        
        MetroTrackManipulatorFactory factory = new ReplaceFactory();
        getFactoryMap().addFactory( "replace-tracks", factory );
        getFactoryMap().addFactory( "replace" , factory );
        getFactoryMap().addFactory( "rept" , factory );
    }
}
