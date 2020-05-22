package metro;

import java.util.Collection;
import java.util.List;

import lamu.lib.Invokable;

public class MetroTrackSelectorBasic {
    public static MetroTrackSelector allSelector() {
        return new MetroTrackSelector() {
            @Override
            public void selectTracks(List<MetroTrack> tracks, List<MetroTrack> selectedTracks) {
                selectedTracks.addAll(tracks);
            }
            @Override
            public String toString() {
                return "[MetroTrackSelector all]";
            }
        };
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
                return "[MetroTrackSelector invokable:("+ invokable +") ]";
            }
        };
    }
    
}
