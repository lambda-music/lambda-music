package metro;

import java.util.Collection;
import java.util.List;

import lamu.lib.Invokable;

public class MetroTrackSelector {
    public static MetroSelector<MetroTrack> allSelector() {
        return new MetroSelector<MetroTrack>() {
            @Override
            public void selectTracks(List<MetroTrack> objects, List<MetroTrack> selectedObjects) {
                selectedObjects.addAll(objects);
            }
            @Override
            public String toString() {
                return "[MetroTrackSelector all]";
            }
        };
    }
    public static final MetroSelector<MetroTrack> nameSelector( Object name ) {
        return new MetroSelector<MetroTrack>() {
            @Override
            public void selectTracks(List<MetroTrack> objects, List<MetroTrack> selectedObjects) {
                for ( MetroTrack track : objects ) {
                    if ( name.equals( track.getName() ) ) {
                        selectedObjects.add(track);
                    }
                }
            }
            @Override
            public String toString() {
                return "[MetroTrackSelector name:("+name+")]";
            }
        };
    }
    public static final MetroSelector<MetroTrack> nameSelector( Collection<? extends Object> names ) {
        return new MetroSelector<MetroTrack>() {
            @Override
            public void selectTracks(List<MetroTrack> objects, List<MetroTrack> selectedObjects) {
                for ( MetroTrack track : objects ) {
                    for ( Object name : names ) {
                        if ( name.equals( track.getName())) {
                            selectedObjects.add(track);
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
    public static final MetroSelector<MetroTrack> tagOrSelector( Collection<? extends Object> tags ) {
        return new MetroSelector<MetroTrack>() {
            @Override
            public void selectTracks(List<MetroTrack> objects, List<MetroTrack> selectedObjects) {
                for ( MetroTrack track : objects ) {
                    for ( Object tag : tags ) {
                        if ( tag.equals( track.getName() ) || track.getTags().contains( tag )) {
                            selectedObjects.add(track);
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
    public static final MetroSelector<MetroTrack> tagAndSelector( Collection<? extends Object> tags ) {
        return new MetroSelector<MetroTrack>() {
            @Override
            public void selectTracks(List<MetroTrack> objects, List<MetroTrack> selectedObjects) {
                for ( MetroTrack track : objects ) {
                    boolean flag = true;
                    for ( Object tag : tags ) {
                        if ( tag.equals( track.getName() ) || track.getTags().contains( tag )) {
                            continue;
                        }
                        flag = false;
                        break;
                    }
                    if ( flag )
                        selectedObjects.add(track);
                }
            }
            @Override
            public String toString() {
                return "[MetroTrackSelector tags-and:("+tags+"]";
            }
        };
    }
    public static final MetroSelector<MetroTrack> constant( MetroTrack track ) {
        return new MetroSelector<MetroTrack>() {
            @Override
            public void selectTracks(List<MetroTrack> objects, List<MetroTrack> selectedObjects) {
                selectedObjects.add( track );
            }
            @Override
            public String toString() {
                return "[MetroTrackSelector track:("+track+")]";
            }
        };
    }
    public static final MetroSelector<MetroTrack> constant( Collection<MetroTrack> tracks ) {
        return new MetroSelector<MetroTrack>() {
            @Override
            public void selectTracks(List<MetroTrack> objects, List<MetroTrack> selectedObjects) {
                selectedObjects.addAll( tracks );
            }
            @Override
            public String toString() {
                return "[MetroTrackSelector tracks:(" + tracks + ")]";
            }
        };
    }

    public static final MetroSelector<MetroTrack> createInvokableSelector( Invokable invokable ) {
        return new MetroSelector<MetroTrack>() {
            @Override
            public void selectTracks(List<MetroTrack> objects, List<MetroTrack> selectedObjects) {
                invokable.invoke( objects, selectedObjects );
            }
            @Override
            public String toString() {
                return "[MetroTrackSelector invokable:("+ invokable +") ]";
            }
        };
    }
    public static final MetroSelector<MetroTrack> createLinewiseInvokableSelector( Invokable invokable ) {
        return new MetroSelector<MetroTrack>() {
            @Override
            public void selectTracks(List<MetroTrack> objects, List<MetroTrack> selectedObjects) {
                for ( MetroTrack track : objects ) {
                    if ( Boolean.FALSE.equals( invokable.invoke( track.getName(), track.getTags()) ) ) {
                        continue;
                    } else {
                        selectedObjects.add( track );
                    }
                }
            }
            @Override
            public String toString() {
                return "[MetroTrackSelector invokable:("+ invokable +") ]";
            }
        };
    }
    
}
