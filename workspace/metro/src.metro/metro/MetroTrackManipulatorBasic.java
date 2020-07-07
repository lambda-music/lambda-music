package metro;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.logging.Level;

import lamu.lib.Invokable;
import lamu.lib.logging.Logger;

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
            public MetroTrackManipulator create( Object... args ) {
                logWarn( "*************** AN IDLE MANIPULATOR WAS EXECUTED ***************" );
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
        if ( trackSynchronizer != null )
            trackSelector = MetroTrackSelectorBasic.synchronizedStopper(trackSelector, trackSynchronizer);
        MetroTrackManipulator trackManipulator = removing( trackSelector );
        return trackManipulator;
    }

    static {
        final class RemovingFactory implements MetroTrackManipulatorFactory {
            @Override
            public MetroTrackManipulator create(Object... args) {
                MetroTrackSelector     trackSelector     = 0 < args.length ? (MetroTrackSelector)args[0]     : null;
                MetroTrackSynchronizer trackSynchronizer = 1 < args.length ? (MetroTrackSynchronizer)args[1] : null;

                return removing( trackSelector, trackSynchronizer );
            }
        }
        MetroTrackManipulatorFactory factory = new RemovingFactory();
        getFactoryMap().addFactory( "remove-tracks", factory );
        getFactoryMap().addFactory( "remove" , factory );
        getFactoryMap().addFactory( "remt" , factory );
        getFactoryMap().addFactory( "r" , factory );
    }

    
    public static MetroTrackManipulator register(MetroTrackSelector selector) {
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
            public MetroTrackManipulator create( Object... args ) {
                MetroTrackSelector     trackSelector     = 0 < args.length ? (MetroTrackSelector)args[0]     : null;
                MetroTrackSynchronizer trackSynchronizer = 1 < args.length ? (MetroTrackSynchronizer)args[1] : null;
                MetroTrackManipulator trackManipulator = register( trackSelector );
                
                if ( trackSynchronizer != null )
                    trackSelector = MetroTrackSelectorBasic.synchronizedStarter(trackSelector, trackSynchronizer );

                return trackManipulator;
            }
        }
        MetroTrackManipulatorFactory factory = new RegisteringFactory();
        getFactoryMap().addFactory( "put-tracks", factory );
        getFactoryMap().addFactory( "put" , factory );
        getFactoryMap().addFactory( "putt" , factory );
        getFactoryMap().addFactory( "p" , factory );
    }


    public static MetroTrackManipulator unregister(MetroTrackSelector selector) {
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
            public MetroTrackManipulator create(Object... args) {
                MetroTrackSelector     trackSelector     = 0 < args.length ? (MetroTrackSelector)args[0]     : null;
                MetroTrackSynchronizer trackSynchronizer = 1 < args.length ? (MetroTrackSynchronizer)args[1] : null;

                if ( trackSynchronizer != null )
                    trackSelector = MetroTrackSelectorBasic.synchronizedStopper(trackSelector, trackSynchronizer );
                MetroTrackManipulator trackManipulator = unregister( trackSelector );
                return trackManipulator;
            }
        }
        MetroTrackManipulatorFactory factory = new UnregisteringFactory();
        getFactoryMap().addFactory( "kill-tracks", factory );
        getFactoryMap().addFactory( "kill" , factory );
        getFactoryMap().addFactory( "kilt" , factory );
        getFactoryMap().addFactory( "k" , factory );
    }

    
    @Deprecated
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

    @Deprecated
    public static MetroTrackManipulator synchronizedStarter(
        MetroTrackManipulator manipulator,
        MetroTrackSynchronizer trackSynchronizer) 
    {
        if (trackSynchronizer == null)
            return manipulator;
        else
            return new MetroTrackSynchronizedStarter(manipulator, trackSynchronizer);
    }

    @Deprecated
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

    @Deprecated
    public static MetroTrackManipulator synchronizedStopper( MetroTrackManipulator manipulator, MetroTrackSynchronizer trackSynchronizer) {
        if (trackSynchronizer == null)
            return manipulator;
        else
            return new MetroTrackSynchronizedStopper(manipulator, trackSynchronizer);
    }
    
    
    ///
    
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

    
    public static MetroTrackManipulator replace( 
        MetroTrackSelector trackSelector, 
        MetroTrackSynchronizer startSynchronizer, 
        MetroTrackSynchronizer stopSynchronizer )  
    {
        ArrayList<MetroTrack> selectedTracks = new ArrayList<>();
        MetroTrackSelector.resolveSelector( Arrays.asList( trackSelector ) ,selectedTracks );
        return replace( selectedTracks, startSynchronizer, stopSynchronizer );
    }

    public static MetroTrackManipulator replace( 
        List<MetroTrack> tracks, 
        MetroTrackSynchronizer startSynchronizer, 
        MetroTrackSynchronizer stopSynchronizer)  
    {
        return multiple(
                    MetroTrackManipulatorBasic.removing( 
                        MetroTrackSelectorBasic.synchronizedStopper(
                            MetroTrackSelectorBasic.correspondingNamedTrack(tracks),
                            stopSynchronizer)),
                
                    MetroTrackManipulatorBasic.register(
                        MetroTrackSelectorBasic.synchronizedStarter(
                            MetroTrackSelectorBasic.trackConstant(tracks), 
                            startSynchronizer )));
    }

    static {
    	final class ReplaceFactory implements MetroTrackManipulatorFactory {
    		/**
    		 * @param args
    		 * <ul>
    		 * <li>[0] track selector</li>
    		 * <li>[1] start synchronizer</li>
    		 * <li>[2] stop synchronizer</li>
    		 * </ul>
    		 */
    		@Override
    		public MetroTrackManipulator create( Object... args) {
    			MetroTrackSelector     trackSelector     = 0 < args.length ? (MetroTrackSelector)args[0]     : null;
    			MetroTrackSynchronizer startSynchronizer = 1 < args.length ? (MetroTrackSynchronizer)args[1] : null;
    			MetroTrackSynchronizer stopSynchronizer  = 2 < args.length ? (MetroTrackSynchronizer)args[2] : null;
    			return replace( trackSelector, startSynchronizer, stopSynchronizer );
    		}
    	}
        
        MetroTrackManipulatorFactory factory = new ReplaceFactory();
        getFactoryMap().addFactory( "replace-tracks", factory );
        getFactoryMap().addFactory( "replace" , factory );
        getFactoryMap().addFactory( "rept" , factory );
    }
    
    static class ProcessTrackManipulator implements MetroTrackManipulator {
		private MetroTrackSelector trackSelector;
		private Invokable invokable;
		public ProcessTrackManipulator(MetroTrackSelector trackSelector, Invokable invokable) {
			this.trackSelector = trackSelector;
			this.invokable = invokable;
		}

		@Override
		public void manipulateTracks(
				List<MetroTrack> currentTracks, List<MetroTrack> registeringTracks,
				List<MetroTrack> removingTracks, List<MetroTrack> unregisteringTracks) 
		{
			List<MetroTrack> selectedTracks = new ArrayList<MetroTrack>();
			trackSelector.selectTracks(currentTracks, selectedTracks);
			invokable.invoke((Object[])selectedTracks.toArray());
		}
    }
    
    static {
    	final class ExecuteFactory implements MetroTrackManipulatorFactory {
    		/**
    		 * @param args
    		 * [0] Invokable object
    		 */
    		@Override
    		public MetroTrackManipulator create( Object... args) {
    			MetroTrackSelector     trackSelector     = 0 < args.length ? (MetroTrackSelector)args[0]  : null;
    			Invokable              invokable         = 1 < args.length ? (Invokable)args[1]           : null;
    			if ( trackSelector == null )
    				throw new IllegalArgumentException("track selecter == null");
    			if ( invokable == null )
    				throw new IllegalArgumentException( "invokable == null" );
    			
    			return new ProcessTrackManipulator( trackSelector, invokable );
    		}
    	}
        
        MetroTrackManipulatorFactory factory = new ExecuteFactory();
        getFactoryMap().addFactory( "process-tracks", factory );
        getFactoryMap().addFactory( "process", factory );
        getFactoryMap().addFactory( "proc" , factory );
    }
    
    
    
}
