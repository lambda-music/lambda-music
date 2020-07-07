package metro;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.logging.Level;

import lamu.lib.Invokable;
import lamu.lib.logging.Logger;

public class MetroTrackSelectorBasic {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }


    private static final MetroFactoryMap<MetroTrackSelectorFactory> factoryMap = new MetroFactoryMap<MetroTrackSelectorFactory>();
    public static MetroFactoryMap<MetroTrackSelectorFactory> getFactoryMap() {
        return factoryMap;
    }



    static final class MetroTrackSelectorNone implements MetroTrackSelector {
        static final MetroTrackSelector INSTANCE = new MetroTrackSelectorNone();
        @Override
        public void selectTracks(List<MetroTrack> currentTracks, List<MetroTrack> selectedTracks) {
        }
        @Override
        public String toString() {
            return "(MetroTrackSelector type: 'none)";
        }
    }
    public static MetroTrackSelector none() {
        return MetroTrackSelectorNone.INSTANCE;
    }
    static {
        final class NoneFactory implements MetroTrackSelectorFactory {
            @Override
            public MetroTrackSelector create(Object[] args) {
                return none();
            }
        }
        MetroTrackSelectorFactory factory = new NoneFactory();
        getFactoryMap().addFactory( "none" , factory );
        getFactoryMap().addFactory( "no" , factory );
    }

    static final class MetroTrackSelectorAll implements MetroTrackSelector {
        static final MetroTrackSelector INSTANCE = new MetroTrackSelectorAll();
        @Override
        public void selectTracks(List<MetroTrack> currentTracks, List<MetroTrack> selectedTracks) {
            selectedTracks.addAll(currentTracks);
        }

        @Override
        public String toString() {
            return "(MetroTrackSelector type: 'all)";
        }
    }
    public static MetroTrackSelector all() {
        return MetroTrackSelectorAll.INSTANCE;
    }
    
    static {
        final class AllFactory implements MetroTrackSelectorFactory {
            @Override
            public MetroTrackSelector create(Object[] args) {
                return all();
            }
        }
        MetroTrackSelectorFactory factory = new AllFactory();
        getFactoryMap().addFactory( "all" , factory );
        getFactoryMap().addFactory( "a" , factory );
    }
    
    public static final MetroTrackSelector name( Object name ) {
        class SingleNameSelector implements MetroTrackSelector {
            private final Object name;
            public SingleNameSelector(Object name) {
                this.name = name;
            }
            @Override
            public void selectTracks(List<MetroTrack> currentTracks, List<MetroTrack> selectedTracks) {
                for ( MetroTrack track : currentTracks ) {
                    if ( Objects.equals( name, track.getName())) {
                        selectedTracks.add(track);
                    }
                }
            }
            @Override
            public String toString() {
                return String.format( "(MetroTrackSelector type: 'name-single value: %s)", name );
            }
        }
        return new SingleNameSelector(name);
    }   
    public static final MetroTrackSelector names( Collection<? extends Object> names ) {
        final class MultipleNameSelector implements MetroTrackSelector {
            private final Collection<? extends Object> names;
            public MultipleNameSelector(Collection<? extends Object> names) {
                this.names = names;
            }
            @Override
            public void selectTracks(List<MetroTrack> currentTracks, List<MetroTrack> selectedTracks) {
                for ( MetroTrack track : currentTracks ) {
                    for ( Object name : names ) {
                        if ( Objects.equals( name, track.getName())) {
                            selectedTracks.add(track);
                            break;
                        }
                    }
                }
            }
            @Override
            public String toString() {
                return String.format( "(MetroTrackSelector type: 'name-multiple value: %s)", MetroUtils.listToString(names));
            }
        }
        return new MultipleNameSelector(names);
    }
    static {
        final class NameFactory implements MetroTrackSelectorFactory {
            @Override
            public MetroTrackSelector create(Object[] args) {
                List<Object> list = MetroTrackSelectorFactory.processArgs(args);
                if (list.size() == 0 ) {
                    return none();
                } else if (list.size() == 1 ) {
                    return name( list.get(0));
                } else {
                    return names( list );
                }
            }
        }
        MetroTrackSelectorFactory factory = new NameFactory();
        getFactoryMap().addFactory( "name" , factory );
        getFactoryMap().addFactory( "n" , factory );
    }
    
    
    public static final MetroTrackSelector tagOr( Collection<? extends Object> tags ) {
        final class TagOrSelector implements MetroTrackSelector {
            private final Collection<? extends Object> tags;
            public TagOrSelector(Collection<? extends Object> tags) {
                this.tags = tags;
            }
            @Override
            public void selectTracks(List<MetroTrack> currentTracks, List<MetroTrack> selectedTracks) {
                for ( MetroTrack track : currentTracks ) {
                    for ( Object tag : tags ) {
                        if ( tag.equals( track.getName() ) || track.getTags().contains( tag )) {
                            selectedTracks.add(track);
                            break;
                        }
                    }
                }
            }
            @Override
            public String toString() {
                return String.format( "(MetroTrackSelector type: 'tags-or value: %s)", MetroUtils.listToString(tags));
            }
        }
        return new TagOrSelector(tags);
    }
    static {
        final class TagOrFactory implements MetroTrackSelectorFactory {
            @Override
            public MetroTrackSelector create(Object[] args) {
                return tagOr(MetroTrackSelectorFactory.processArgs(args));
            }
        }
        MetroTrackSelectorFactory factory = new TagOrFactory();
        getFactoryMap().addFactory( "tag-or" , factory );
        getFactoryMap().addFactory( "tago" , factory );
        getFactoryMap().addFactory( "tag" , factory );
        getFactoryMap().addFactory( "t" , factory );
    }
    
    public static final MetroTrackSelector tagAnd( Collection<? extends Object> tags ) {
        final class TagAndSelector implements MetroTrackSelector {
            private final Collection<? extends Object> tags;
            public TagAndSelector(Collection<? extends Object> tags) {
                this.tags = tags;
            }
            @Override
            public void selectTracks(List<MetroTrack> currentTracks, List<MetroTrack> selectedTracks) {
                for ( MetroTrack track : currentTracks ) {
                    boolean flag = true;
                    for ( Object tag : tags ) {
                        if ( tag.equals( track.getName() ) || track.getTags().contains( tag )) {
                            continue;
                        }
                        flag = false;
                        break;
                    }
                    if ( flag )
                        selectedTracks.add(track);
                }
            }
            @Override
            public String toString() {
                return String.format( "(MetroTrackSelector type: 'tags-and value: %s)", MetroUtils.listToString(tags) );
            }
        }
        return new TagAndSelector(tags);
    }
    static {
        final class TagAndFactory implements MetroTrackSelectorFactory {
            @Override
            public MetroTrackSelector create(Object[] args) {
                return tagAnd(MetroTrackSelectorFactory.processArgs(args));
            }
        }
        MetroTrackSelectorFactory factory = new TagAndFactory();
        getFactoryMap().addFactory( "tag-and" , factory );
        getFactoryMap().addFactory( "taga" , factory );
        getFactoryMap().addFactory( "ta" , factory );
    }
    
    
    public static final MetroTrackSelector constant( MetroTrack track ) {
        final class ConstantSelector implements MetroTrackSelector {
            private final MetroTrack track;
            public ConstantSelector(MetroTrack track) {
                this.track = track;
            }
            @Override
            public void selectTracks(List<MetroTrack> currentTracks, List<MetroTrack> selectedTracks) {
                selectedTracks.add( track );
            }
            @Override
            public String toString() {
                return "[MetroTrackSelector track:("+track+")]";
            }
        }
        return new ConstantSelector(track);
    }
    public static final MetroTrackSelector trackConstant( Collection<MetroTrack> tracks ) {
        final class ConstantListSelector implements MetroTrackSelector {
            private final Collection<MetroTrack> tracks;
            public ConstantListSelector(Collection<MetroTrack> tracks) {
                this.tracks = tracks;
            }
            @Override
            public void selectTracks(List<MetroTrack> currentTracks, List<MetroTrack> selectedTracks) {
                selectedTracks.addAll( tracks );
            }
            @Override
            public String toString() {
                return String.format( "(MetroTrackSelector type: 'track-constant value: %s)", 
                    MetroUtils.listToString( tracks ) );
            }
            
        }
        return new ConstantListSelector(tracks);
    }
    static {
        final class TrackConstantFactory implements MetroTrackSelectorFactory {
            @Override
            public MetroTrackSelector create(Object[] args) {
                List<MetroTrack> argList = 
                    Collections.checkedList(
                        MetroTrackSelectorFactory.processArgs(args),
                        MetroTrack.class );
                
                if ( argList.isEmpty() ) {
                    return none();
                } else if ( argList.size() == 1 ) {
                    return constant(argList.get(0));
                } else {
                    return trackConstant(argList);
                }
            }
        }
        MetroTrackSelectorFactory factory = new TrackConstantFactory();
        getFactoryMap().addFactory( "constant" , factory );
        getFactoryMap().addFactory( "const" , factory );
        getFactoryMap().addFactory( "c" , factory );
    }

    
    public static final MetroTrackSelector trackFactory( MetroTrackFactory trackFactory ) {
        final class TrackFactorySelector implements MetroTrackSelector {
            private final MetroTrackFactory trackFactory;
            public TrackFactorySelector(MetroTrackFactory trackFactory) {
                this.trackFactory = trackFactory;
            }

            @Override
            public void selectTracks(List<MetroTrack> currentTracks, List<MetroTrack> selectedTracks) {
                selectedTracks.add( trackFactory.createTrack() );
            }

            @Override
            public String toString() {
                return String.format( "(MetroTrackSelector type: 'newt-single value: %s)", trackFactory );
            }
        }
        return new TrackFactorySelector(trackFactory);
    }
    
    public static final MetroTrackSelector trackFactoryList( Collection<MetroTrackFactory> trackFactories ) {
        final class TrackFactoryListSelector implements MetroTrackSelector {
            private final Collection<MetroTrackFactory> trackFactories;
            public TrackFactoryListSelector(Collection<MetroTrackFactory> trackFactories) {
                this.trackFactories = trackFactories;
            }

            @Override
            public void selectTracks(List<MetroTrack> currentTracks, List<MetroTrack> selectedTracks) {
                ArrayList<MetroTrack> tracks = new ArrayList<>(trackFactories.size());
                for ( MetroTrackFactory f : trackFactories ) {
                    tracks.add( f.createTrack() );
                }
                selectedTracks.addAll( tracks );
            }

            @Override
            public String toString() {
                return String.format( "(MetroTrackSelector type: 'newt-multiple value: %s)",
                    MetroUtils.listToString(trackFactories) );
            }
        }
        return new TrackFactoryListSelector(trackFactories);
    }
    static {
        final class TrackFactoryFactory implements MetroTrackSelectorFactory {
            @Override
            public MetroTrackSelector create(Object[] args) {
                List<MetroTrackFactory> argList = 
                    Collections.checkedList(
                        MetroTrackSelectorFactory.processArgs(args),
                        MetroTrackFactory.class );
                
                return trackFactoryList( argList );
            }
        }
        MetroTrackSelectorFactory factory = new TrackFactoryFactory();
        getFactoryMap().addFactory( "factory" , factory );
        getFactoryMap().addFactory( "fact" , factory );
        getFactoryMap().addFactory( "f" , factory );
        getFactoryMap().addFactory( "new-track" , factory );
        getFactoryMap().addFactory( "newt" , factory );
    }


    
    public static final MetroTrackSelector correspondingNamedTrack( Collection<MetroTrack> tracks ) {
        final class CorrespondingNamedTrack implements MetroTrackSelector {
            private final Collection<MetroTrack> tracks;
            public CorrespondingNamedTrack(Collection<MetroTrack> tracks) {
                this.tracks = tracks;
            }
            @Override
            public void selectTracks(List<MetroTrack> currentTracks, List<MetroTrack> selectedTracks) {
                for ( MetroTrack ct : currentTracks ) {
                    for ( MetroTrack t : tracks ) {
                        if (Objects.equals(ct.getName(),t.getName())) {
                            selectedTracks.add( ct );
                            break;
                        }
                    }
                }
            }
            @Override
            public String toString() {
                return String.format( 
                    "(MetroTrackSelector type: corresponding-tracks-direct value: %s)", 
                    MetroUtils.listToString( tracks ) );
            }

        }
        return new CorrespondingNamedTrack(tracks);
    }

    public static final MetroTrackSelector correspondingNamedTrack( MetroTrackSelector trackSelector ) {
        final class CorrespondingNamedTrack implements MetroTrackSelector {
            private final MetroTrackSelector trackSelector ;
            public CorrespondingNamedTrack(MetroTrackSelector trackSelector) {
                this.trackSelector = trackSelector;
            }
            @Override
            public void selectTracks(List<MetroTrack> currentTracks, List<MetroTrack> selectedTracks) {
                
                // 1. Execute the specified track selector.
                List<MetroTrack> tracks = new ArrayList<MetroTrack>();
                trackSelector.selectTracks(currentTracks, tracks);
                
                // 2. Look up the matched track objects and add them to the selectedTracks.
                for ( MetroTrack ct : currentTracks ) {
                    for ( MetroTrack t : tracks ) {
                        if (Objects.equals(ct.getName(),t.getName())) {
                            selectedTracks.add( ct );
                            break;
                        }
                    }
                }
            }
            @Override
            public String toString() {
                return String.format( "(MetroTrackSelector type: 'corresponding-tracks-indirect value: %s)", trackSelector );
            }
        }
        return new CorrespondingNamedTrack(trackSelector);
    }

    
    static {
        final class CorrespondingFactory implements MetroTrackSelectorFactory {
            @Override
            public MetroTrackSelector create(Object[] args) {
//                Object arg0 = args[0];
//                if ( arg0 instanceof Collection ) {
//                    return correspondingNamedTrack((Collection)arg0 );
//                } else {
//                    return correspondingNamedTrack(Arrays.asList((MetroTrack)arg0));
//                }
                List<MetroTrack> argList = 
                    Collections.checkedList(
                        MetroTrackSelectorFactory.processArgs(args),
                        MetroTrack.class );
                return correspondingNamedTrack( argList );

            }
        }
        MetroTrackSelectorFactory factory = new CorrespondingFactory();
        getFactoryMap().addFactory( "corresponding" , factory );
        getFactoryMap().addFactory( "corresp" , factory );
        getFactoryMap().addFactory( "cor" , factory );
    }


    public static final MetroTrackSelector invokable( Invokable invokable ) {
        final class InvokableSelector implements MetroTrackSelector {
            private final Invokable invokable;
            public InvokableSelector(Invokable invokable) {
                this.invokable = invokable;
            }
            @Override
            public void selectTracks(List<MetroTrack> currentTracks, List<MetroTrack> selectedTracks) {
                invokable.invoke( currentTracks, selectedTracks );
            }
            @Override
            public String toString() {
                return String.format( "(MetroTrackSelector type: 'invokable value: %s)", invokable );
            }
        }
        return new InvokableSelector(invokable);
    }
    static {
        final class InvokableFactory implements MetroTrackSelectorFactory {
            @Override
            public MetroTrackSelector create(Object[] args) {
                MetroTrackSelectorFactory.checkArgs(args, 1);
                Object arg0 = args[0];
                return invokable( (Invokable)arg0);
            }
        }
        MetroTrackSelectorFactory factory = new InvokableFactory();
        getFactoryMap().addFactory( "execution" , factory );
        getFactoryMap().addFactory( "exec" , factory );
        getFactoryMap().addFactory( "e" , factory );
    }

    public static final MetroTrackSelector linewiseInvokable( Invokable invokable ) {
        final class LinewiseInvokableSelector implements MetroTrackSelector {
            private final Invokable invokable;
            private LinewiseInvokableSelector(Invokable invokable) {
                this.invokable = invokable;
            }
            @Override
            public void selectTracks(List<MetroTrack> currentTracks, List<MetroTrack> selectedTracks) {
                for ( MetroTrack track : currentTracks ) {
                    if ( Boolean.FALSE.equals( invokable.invoke( track.getName(), track.getTags()) ) ) {
                        continue;
                    } else {
                        selectedTracks.add( track );
                    }
                }
            }
            @Override
            public String toString() {
                return String.format( "(MetroTrackSelector type: 'line-invokable value: %s)", invokable );
            }
        }
        return new LinewiseInvokableSelector(invokable);
    }
    static {
        final class LinewiseInvokableFactory implements MetroTrackSelectorFactory {
            @Override
            public MetroTrackSelector create(Object[] args) {
                MetroTrackSelectorFactory.checkArgs(args, 1);
                Object arg0 = args[0];
                return linewiseInvokable( (Invokable)arg0);
            }
        }
        MetroTrackSelectorFactory factory = new LinewiseInvokableFactory();
        getFactoryMap().addFactory( "line-execution" , factory );
        getFactoryMap().addFactory( "lexec" , factory );
        getFactoryMap().addFactory( "le" , factory );
    }
    
    static abstract class SynchronizerSelector implements MetroTrackSelector {
        private final MetroTrackSelector trackSelector;
        private MetroTrackSynchronizer trackSynchronizer;
        public SynchronizerSelector(MetroTrackSelector trackSelector, MetroTrackSynchronizer trackSynchronzier ) {
            this.trackSelector = trackSelector;
            this.trackSynchronizer = trackSynchronzier;
        }

        @Override
        public void selectTracks(List<MetroTrack> currentTracks, List<MetroTrack> selectedTracks) {
            trackSelector.selectTracks(currentTracks, selectedTracks );
            for ( MetroTrack ct : selectedTracks ) {
                 proc( ct.getSequence(), trackSynchronizer );
            }
        }

        protected abstract void proc(MetroSequence sequence, MetroTrackSynchronizer trackSynchronizer);
        @Override
        public String toString() {
            return String.format( "(MetroTrackSelector type: 'synchronizer value: %s sync-value: %s)",  trackSelector , trackSynchronizer );
        }
    }
    
    public static final MetroTrackSelector synchronizedStarter( MetroTrackSelector trackSelector, MetroTrackSynchronizer trackSynchronizer ) {
        final class StartSynchronizerSelector extends SynchronizerSelector {
            public StartSynchronizerSelector(MetroTrackSelector trackSelector, MetroTrackSynchronizer trackSynchronzier) {
                super(trackSelector, trackSynchronzier);
            }
            @Override
            protected void proc(MetroSequence sequence, MetroTrackSynchronizer trackSynchronizer) {
                if ( sequence instanceof MetroSynchronizedStarter ) {
                    ((MetroSynchronizedStarter)sequence).setStartSynchronizer(trackSynchronizer);
                }
            }
        }
        return new StartSynchronizerSelector(trackSelector, trackSynchronizer);
    }

    
    static {
        final class StartSynchronizerFactory implements MetroTrackSelectorFactory {
            @Override
            public MetroTrackSelector create(Object[] args) {
                MetroTrackSelectorFactory.checkArgs(args, 2);
                return synchronizedStarter((MetroTrackSelector) args[0],(MetroTrackSynchronizer) args[1] );
            }
        }
        MetroTrackSelectorFactory factory = new StartSynchronizerFactory();
        getFactoryMap().addFactory( "start-synchronizer" , factory );
        getFactoryMap().addFactory( "start-sync" , factory );
    }

    public static final MetroTrackSelector synchronizedStopper( MetroTrackSelector trackSelector, MetroTrackSynchronizer trackSynchronizer ) {
        final class StopSynchronizerSelector extends SynchronizerSelector {
            public StopSynchronizerSelector(MetroTrackSelector trackSelector, MetroTrackSynchronizer trackSynchronzier) {
                super(trackSelector, trackSynchronzier);
            }
            @Override
            protected void proc(MetroSequence sequence, MetroTrackSynchronizer trackSynchronizer) {
                if ( sequence instanceof MetroSynchronizedStopper ) {
                    ((MetroSynchronizedStopper)sequence).setStopSynchronizer(trackSynchronizer);
                }
            }
        }
        return new StopSynchronizerSelector(trackSelector, trackSynchronizer);
    }
    
    static {
        final class StopSynchronizerFactory implements MetroTrackSelectorFactory {
            @Override
            public MetroTrackSelector create(Object[] args) {
                MetroTrackSelectorFactory.checkArgs(args, 2);
                return synchronizedStopper((MetroTrackSelector) args[0],(MetroTrackSynchronizer) args[1] );
            }
        }
        MetroTrackSelectorFactory factory = new StopSynchronizerFactory();
        getFactoryMap().addFactory( "stop-synchronizer" , factory );
        getFactoryMap().addFactory( "stop-sync" , factory );
    }
}
