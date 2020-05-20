package metro;

import java.util.Collection;
import java.util.List;

import lamu.lib.Invokable;

public class MetroTrackSelector {
    public static final MetroSelector<MetroTrack> createNameSelector( Object name ) {
        return new MetroSelector<MetroTrack>() {
            @Override
            public void selectTracks(List<MetroTrack> objects, List<MetroTrack> selectedObjects) {
                for ( MetroTrack track : objects ) {
                    if ( name.equals( track.getName() ) ) {
                        selectedObjects.add(track);
                    }
                }
            }
        };
    }
    public static final MetroSelector<MetroTrack> createTagOrSelector( Collection<? extends Object> tags ) {
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
        };
    }
    public static final MetroSelector<MetroTrack> createStatic( MetroTrack track ) {
        return new MetroSelector<MetroTrack>() {
            @Override
            public void selectTracks(List<MetroTrack> objects, List<MetroTrack> selectedObjects) {
                selectedObjects.add( track );
            }
        };
    }
    public static final MetroSelector<MetroTrack> createStatic( Collection<MetroTrack> tracks ) {
        return new MetroSelector<MetroTrack>() {
            @Override
            public void selectTracks(List<MetroTrack> objects, List<MetroTrack> selectedObjects) {
                selectedObjects.addAll( tracks );
            }
        };
    }

    public static final MetroSelector<MetroTrack> createInvokableSelector( Invokable invokable ) {
        return new MetroSelector<MetroTrack>() {
            @Override
            public void selectTracks(List<MetroTrack> objects, List<MetroTrack> selectedObjects) {
                invokable.invoke( objects, selectedObjects );
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
        };
    }
    
}
