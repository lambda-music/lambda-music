package metro;

import java.lang.invoke.MethodHandles;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.logging.Level;

import lamu.lib.Invokable;
import lamu.lib.log.Logger;

public class MetroTrackSelectorBasic {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }

    
    private static HashMap<String,MetroTrackSelectorFactory> factoryMap = new HashMap<>();
    public static MetroTrackSelectorFactory getFactory( String name ) {
        if ( ! factoryMap.containsKey(name))
            throw new IllegalArgumentException("unknown factory name (" + name + ")");
        return factoryMap.get(name);
    }
    public static void addFactory( String name, MetroTrackSelectorFactory factory ) {
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

    static final class MetroTrackSelectorAll implements MetroTrackSelector {
        static final MetroTrackSelector INSTANCE = new MetroTrackSelectorAll();
        @Override
        public void selectTracks(List<MetroTrack> tracks, List<MetroTrack> selectedTracks) {
            selectedTracks.addAll(tracks);
        }

        @Override
        public String toString() {
            return "[MetroTrackSelector all]";
        }
    }
    public static MetroTrackSelector allSelector() {
        return MetroTrackSelectorAll.INSTANCE;
    }
    static {
        final class AllFactory implements MetroTrackSelectorFactory {
            @Override
            public MetroTrackSelector createSelector(Object[] args) {
                return MetroTrackSelectorAll.INSTANCE;
            }
        }
        MetroTrackSelectorFactory factory = new AllFactory();
        addFactory( "all" , factory );
        addFactory( "a" , factory );
    }
    
    public static final MetroTrackSelector nameSelector( Object name ) {
        return new MetroTrackSelector() {
            @Override
            public void selectTracks(List<MetroTrack> tracks, List<MetroTrack> selectedTracks) {
                for ( MetroTrack track : tracks ) {
                    if ( name.equals( track.getName() ) ) {
                        selectedTracks.add(track);
                    }
                }
            }
            @Override
            public String toString() {
                return "[MetroTrackSelector name:("+name+")]";
            }
        };
    }   
    public static final MetroTrackSelector nameSelector( Collection<? extends Object> names ) {
        return new MetroTrackSelector() {
            @Override
            public void selectTracks(List<MetroTrack> tracks, List<MetroTrack> selectedTracks) {
                for ( MetroTrack track : tracks ) {
                    for ( Object name : names ) {
                        if ( name.equals( track.getName())) {
                            selectedTracks.add(track);
                            break;
                        }
                    }
                }
            }
            @Override
            public String toString() {
                return "[MetroTrackSelector names:("+names+")]";
            }
        };
    }
    static {
        final class NameFactory implements MetroTrackSelectorFactory {
            @Override
            public MetroTrackSelector createSelector(Object[] args) {
                argumentCheck(args, 1);
                Object arg0 = args[0];
                if ( arg0 instanceof Collection ) {
                    return nameSelector((Collection)arg0 );
                } else {
                    return nameSelector(arg0);
                }
            }
        }
        MetroTrackSelectorFactory factory = new NameFactory();
        addFactory( "name" , factory );
        addFactory( "n" , factory );
    }
    
    
    public static final MetroTrackSelector tagOrSelector( Collection<? extends Object> tags ) {
        return new MetroTrackSelector() {
            @Override
            public void selectTracks(List<MetroTrack> tracks, List<MetroTrack> selectedTracks) {
                for ( MetroTrack track : tracks ) {
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
                return "[MetroTrackSelector tags-or:("+tags+"]";
            }
        };
    }
    static {
        final class TagOrFactory implements MetroTrackSelectorFactory {
            @Override
            public MetroTrackSelector createSelector(Object[] args) {
                argumentCheck(args, 1);
                Object arg0 = args[0];
                if ( arg0 instanceof Collection ) {
                    return tagOrSelector((Collection)arg0 );
                } else {
                    return tagOrSelector( Arrays.asList(arg0));
                }
            }
        }
        MetroTrackSelectorFactory factory = new TagOrFactory();
        addFactory( "tag-or" , factory );
        addFactory( "tago" , factory );
        addFactory( "tag" , factory );
        addFactory( "t" , factory );
    }
    
    public static final MetroTrackSelector tagAndSelector( Collection<? extends Object> tags ) {
        return new MetroTrackSelector() {
            @Override
            public void selectTracks(List<MetroTrack> tracks, List<MetroTrack> selectedTracks) {
                for ( MetroTrack track : tracks ) {
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
                return "[MetroTrackSelector tags-and:("+tags+"]";
            }
        };
    }
    static {
        final class TagAndFactory implements MetroTrackSelectorFactory {
            @Override
            public MetroTrackSelector createSelector(Object[] args) {
                argumentCheck(args, 1);
                Object arg0 = args[0];
                if ( arg0 instanceof Collection ) {
                    return tagAndSelector((Collection)arg0 );
                } else {
                    return tagAndSelector( Arrays.asList(arg0));
                }
            }
        }
        MetroTrackSelectorFactory factory = new TagAndFactory();
        addFactory( "tag-and" , factory );
        addFactory( "taga" , factory );
        addFactory( "ta" , factory );
    }
    
    
    public static final MetroTrackSelector constant( MetroTrack track ) {
        return new MetroTrackSelector() {
            @Override
            public void selectTracks(List<MetroTrack> tracks, List<MetroTrack> selectedTracks) {
                selectedTracks.add( track );
            }
            @Override
            public String toString() {
                return "[MetroTrackSelector track:("+track+")]";
            }
        };
    }
    public static final MetroTrackSelector constant( Collection<MetroTrack> tracks ) {
        return new MetroTrackSelector() {
            @Override
            public void selectTracks(List<MetroTrack> tracks, List<MetroTrack> selectedTracks) {
                selectedTracks.addAll( tracks );
            }
            @Override
            public String toString() {
                return "[MetroTrackSelector tracks:(" + tracks + ")]";
            }
        };
    }
    static {
        final class ConstantFactory implements MetroTrackSelectorFactory {
            @Override
            public MetroTrackSelector createSelector(Object[] args) {
                argumentCheck(args, 1);
                Object arg0 = args[0];
                if ( arg0 instanceof Collection ) {
                    return constant((Collection)arg0 );
                } else {
                    return constant( (MetroTrack)arg0);
                }
            }
        }
        MetroTrackSelectorFactory factory = new ConstantFactory();
        addFactory( "constant" , factory );
        addFactory( "const" , factory );
        addFactory( "c" , factory );
    }


    public static final MetroTrackSelector createInvokableSelector( Invokable invokable ) {
        return new MetroTrackSelector() {
            @Override
            public void selectTracks(List<MetroTrack> tracks, List<MetroTrack> selectedTracks) {
                invokable.invoke( tracks, selectedTracks );
            }
            @Override
            public String toString() {
                return "[MetroTrackSelector invokable:("+ invokable +") ]";
            }
        };
    }
    static {
        final class ExecutionFactory implements MetroTrackSelectorFactory {
            @Override
            public MetroTrackSelector createSelector(Object[] args) {
                argumentCheck(args, 1);
                Object arg0 = args[0];
                return createInvokableSelector( (Invokable)arg0);
            }
        }
        MetroTrackSelectorFactory factory = new ExecutionFactory();
        addFactory( "execution" , factory );
        addFactory( "exec" , factory );
        addFactory( "e" , factory );
    }

    public static final MetroTrackSelector createLinewiseInvokableSelector( Invokable invokable ) {
        return new MetroTrackSelector() {
            @Override
            public void selectTracks(List<MetroTrack> tracks, List<MetroTrack> selectedTracks) {
                for ( MetroTrack track : tracks ) {
                    if ( Boolean.FALSE.equals( invokable.invoke( track.getName(), track.getTags()) ) ) {
                        continue;
                    } else {
                        selectedTracks.add( track );
                    }
                }
            }
            @Override
            public String toString() {
                return "[MetroTrackSelector line-invokable:("+ invokable +") ]";
            }
        };
    }
    static {
        final class ExecutionFactory implements MetroTrackSelectorFactory {
            @Override
            public MetroTrackSelector createSelector(Object[] args) {
                argumentCheck(args, 1);
                Object arg0 = args[0];
                return createLinewiseInvokableSelector( (Invokable)arg0);
            }
        }
        MetroTrackSelectorFactory factory = new ExecutionFactory();
        addFactory( "line-execution" , factory );
        addFactory( "lexec" , factory );
        addFactory( "le" , factory );
    }
    
}
