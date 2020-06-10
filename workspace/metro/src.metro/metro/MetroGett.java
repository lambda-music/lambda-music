package metro;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

public interface MetroGett extends MetroReft {
    default List<MetroTrack> getTracks() {
        ArrayList<MetroTrack> selectedTracks = new ArrayList<>();
        referTracks( Arrays.asList(MetroTrackSelectorBasic.all()), selectedTracks );
        return selectedTracks;
    }
    default List<MetroTrack> getTracks( Object name ) {
        if ( name instanceof Collection )
            throw new IllegalArgumentException( "This maybe a illegal call to this interface. Call getTracks(Collection)" );
        
        ArrayList<MetroTrack> selectedTracks = new ArrayList<>();
        referTracks( Arrays.asList(MetroTrackSelectorBasic.name(name)), selectedTracks);
        return selectedTracks;
    }
    
    default List<MetroTrack> getTracks( Collection<? extends Object> names ) {
        ArrayList<MetroTrack> selectedTracks = new ArrayList<>();
        referTracks( 
            Arrays.asList(MetroTrackSelectorBasic.names(names)), 
            selectedTracks);
        return selectedTracks;
    }

}
