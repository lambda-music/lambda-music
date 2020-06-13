package metro;

import java.util.ArrayList;

class MetroOutputBuffer {
    long nframes;
    final ArrayList<MetroMidiEvent> eventList;
    public MetroOutputBuffer( int initialBufferSize ) {
        this.eventList = new ArrayList<MetroMidiEvent>(initialBufferSize);
    }
    
    void init( int nframes ) {
        this.eventList.clear();
        this.nframes = nframes;
    }
}
