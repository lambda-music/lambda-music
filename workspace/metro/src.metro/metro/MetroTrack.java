package metro;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

public class MetroTrack {
    public static MetroTrack create( Object name, Collection<Object> tags, MetroSequence sequence ){
        return new MetroTrack( name, tags, sequence );
    }
    public static MetroTrack create( Object name, Collection<Object> tags, MetroSequence sequence,
        MetroTrackSynchronizer startSynchronizer,
        MetroTrackSynchronizer stopSynchronizer )
    {
        MetroTrack metroTrack = new MetroTrack( name, tags, sequence );
        if ( sequence instanceof MetroSynchronizedStarter )
            ((MetroSynchronizedStarter)sequence).setStartSynchronizer(startSynchronizer);
        if ( sequence instanceof MetroSynchronizedStopper )
            ((MetroSynchronizedStopper)sequence).setStopSynchronizer(stopSynchronizer);
        return metroTrack;
    }
    /**
     * Note that the String object which is stored in name field must be interned.  
     */
    private final Object name;
    private final Collection<Object> tags;
    private final MetroSequence sequence;
    public Object getName() {
        return name;
    }
    public Collection<Object> getTags() {
        return tags;
    }
    public MetroSequence getSequence() {
        return sequence;
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
    /**
     * This constructor should be called only by {@link Metro#putTrack(Collection, MetroTrackSynchronizer)}
     *  
     * @param name
     * @param tags
     * @param sequence
     */
    MetroTrack(Object name, Collection<Object> tags, MetroSequence sequence ) {
        super();
        if ( name == null )
            this.name = createUniqueTrackName();
        else
            this.name = checkName( name );
        
        
        if ( tags == null )
            this.tags = Collections.EMPTY_LIST;
        else
            this.tags = new ArrayList( tags );

        if ( sequence == null )
            this.sequence = MetroVoidSequence.getInstance();
        else
            this.sequence = sequence;
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
    private final long uniqueID = (int)(Math.random() * Long.MAX_VALUE);
    long getUniqueID() {
        return uniqueID;
    }
    
    final List<MetroMidiEvent> inputMidiEventList = new ArrayList<>();
    final List<MetroMidiEvent> outputMidiEventList = new ArrayList<>();
    final List<MetroTrack> registeringTrack = new ArrayList<>();
    final List<MetroTrack> unregisteringTrack = new ArrayList<>();

    // ADDED (Thu, 07 May 2020 13:03:35 +0900)    
    /**
     *  TODO
     * @param metro
     * @param trackSynchronizer
     */
    public void remove( Metro metro, MetroTrackSynchronizer trackSynchronizer ) {
        MetroSequence sequence = this.getSequence();
        if ( sequence instanceof MetroSynchronizedStopper ) {
            MetroSynchronizedStopper syncSequence = (MetroSynchronizedStopper)sequence;
            if ( trackSynchronizer != null )
                syncSequence.setStopSynchronizer(trackSynchronizer);
            syncSequence.stop(metro, this);
        } else {
            metro.unregisterTrack(Arrays.asList(this));
        }
    }
}
