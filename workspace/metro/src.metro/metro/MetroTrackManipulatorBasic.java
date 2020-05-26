package metro;

import java.util.List;

public class MetroTrackManipulatorBasic {
    public static MetroTrackManipulator removing(MetroTrackSelector selector) {
        class MetroTrackRemover implements MetroTrackManipulator {
            private final MetroTrackSelector selector;

            public MetroTrackRemover(MetroTrackSelector selector) {
                this.selector = selector;
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
        return new MetroTrackRemover(selector);
    }

    public static MetroTrackManipulator registering(MetroTrackSelector selector) {
        class MetroTrackRemover implements MetroTrackManipulator {
            private final MetroTrackSelector selector;

            public MetroTrackRemover(MetroTrackSelector selector) {
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
        }
        return new MetroTrackRemover(selector);
    }
    public static MetroTrackManipulator unregistering(MetroTrackSelector selector) {
        class MetroTrackRemover implements MetroTrackManipulator {
            private final MetroTrackSelector selector;

            public MetroTrackRemover(MetroTrackSelector selector) {
                this.selector = selector;
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
        return new MetroTrackRemover(selector);
    }

    private static class MetroTrackSynchronizedStarter implements MetroTrackManipulator {
        private final MetroTrackManipulator  manipulator;
        private final MetroTrackSynchronizer trackSynchronizer;

        public MetroTrackSynchronizedStarter(MetroTrackManipulator manipulator,
            MetroTrackSynchronizer trackSynchronizer) {
            this.manipulator       = manipulator;
            this.trackSynchronizer = trackSynchronizer;
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
        MetroTrackSynchronizer trackSynchronizer) {
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

    public static MetroTrackManipulator synchronizedStopper(
        MetroTrackManipulator manipulator,
        MetroTrackSynchronizer trackSynchronizer) {
        if (trackSynchronizer == null)
            return manipulator;
        else
            return new MetroTrackSynchronizedStopper(manipulator, trackSynchronizer);
    }
}
