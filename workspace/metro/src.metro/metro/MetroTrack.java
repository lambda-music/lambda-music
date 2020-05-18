package metro;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

public abstract class MetroTrack implements MetroAbstractTrack {
    /**
     * Note that the String object which is stored in name field must be interned.  
     */
    private final Object name;
    private final Collection<Object> tags;
    @Override
    public Object getName() {
        return name;
    }
    @Override
    public Collection<Object> getTags() {
        return tags;
    }
    private static volatile int uniqueTrackNameCounter = 0;
    private synchronized static String createUniqueTrackName() {
            return "track-" + ( uniqueTrackNameCounter ++ ); 
    }
    
    private static Object checkName( Object name ) {
        if ( name instanceof String ) {
            return ((String)name).intern();
        } else {
            return name;
        }
    }
    public MetroTrack(Object name, Collection<Object> tags) {
        super();
        if ( name == null )
            this.name = createUniqueTrackName();
        else
            this.name = checkName( name );
        
        
        if ( tags == null )
            this.tags = Collections.EMPTY_LIST;
        else
            this.tags = new ArrayList( tags );

    }

    @Override
    public boolean equals(Object obj) {
        if ( false ) {
            if ( obj instanceof MetroTrack ) {
                Object nameA = this.getName();
                Object nameB = ((MetroTrack)obj).getName();
                return  (nameA  == nameB) || nameA .equals(nameB );
            } else {
                return false;
            }
        } else {
            return super.equals( obj );
        }
    }
    @Override
    public int hashCode() {
        if ( false ) {
            return this.getName().hashCode() * 2;
        } else {
            return super.hashCode();
        }
    }
    @Override
    public String toString() {
        return "#" + MetroTrack.class.getSimpleName() + ":" + this.getName() + "#";
    }
    

    /*
     * This value is only for debugging purpose. 
     */
    private final long uniqueID = (int) (Math.random()* Long.MAX_VALUE);
    long getUniqueID() {
        return uniqueID;
    }

    // This method was formerly checkBuffer()
    public abstract void progressBuffer(Metro metro, long measureLengthInFrames) throws MetroException;
    
    public abstract void progressCursor(Metro metro, long nframes, 
        long measureLengthInFrames, 
        List<MetroMidiEvent> inputMidiEventList, 
        List<MetroMidiEvent> outputMidiEventList, 
        List<MetroTrack> tracks,
        List<MetroTrack> registeringTrackList, 
        List<MetroTrack> unregisteringTrackList ) throws MetroException;
    
    final List<MetroMidiEvent> inputMidiEventList = new ArrayList<>();
    final List<MetroMidiEvent> outputMidiEventList = new ArrayList<>();
    final List<MetroTrack> registeringTrack = new ArrayList<>();
    final List<MetroTrack> unregisteringTrack = new ArrayList<>();

    // ADDED (Thu, 07 May 2020 13:03:35 +0900)    
    public void remove(Metro metro, MetroTrackSynchronizer trackSynchronizer ) {
        metro.unregisterTrack(this);
    }
    // TODO ADDED (Sat, 09 May 2020 01:33:54 +0900)
    public void remove(Metro metro, MetroSyncType syncType  ) {
        metro.unregisterTrack(this);
    }

}
