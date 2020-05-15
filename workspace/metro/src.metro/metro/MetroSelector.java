package metro;

import java.util.List;

public interface MetroSelector<T> {
    void selectTracks( List<T> objects, List<T> selectedObjects );
}