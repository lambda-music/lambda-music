package metro;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class MetroMidiFrame {
    public static MetroMidiFrame create( int totalCursor, Collection<MetroMidiEvent> midiEventList ) {
        return new MetroMidiFrame( totalCursor, new ArrayList<>( midiEventList ) );
    }
    int totalCursor;
    List<MetroMidiEvent> midiEventList;
    public MetroMidiFrame(int totalCursor, List<MetroMidiEvent> midiEventList) {
        super();
        this.totalCursor = totalCursor;
        this.midiEventList = midiEventList;
    }
    
    public List<MetroMidiEvent> getMidiEventList() {
        return midiEventList;
    }
    public int getTotalCursor() {
        return totalCursor;
    }
}
