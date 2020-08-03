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
    
    /**
     * Create a track object.
     * 
     * @param name              Specifies the object which represents the track; can
     *                          be any object value of any type.
     * @param tags              Specifies the tag objects. Each tag object can be
     *                          any value of any type. Specify null if no tag is
     *                          specified; the value is automatically replaced with
     *                          an empty list.
     * @param sequence
     * @param trackMode TODO
     * @param startSynchronizer Specifies the start synchronizer for the track; null if no start synchronizer is necessary. 
     *                          See {@link MetroTrackSynchronizer}.               
     * @param stopSynchronizer  Specifies the stop  synchronizer for the track; null if no stop synchronizer is necessary.
     *                          See {@link MetroTrackSynchronizer}.                
     * 
     * @return                  The created track object.
     * 
     */
    public static MetroTrack create( 
        Object name, 
        Collection<Object> tags, 
        MetroSequence sequence,
        MetroTrackMode trackMode,
        MetroTrackSynchronizer startSynchronizer, 
        MetroTrackSynchronizer stopSynchronizer )
    {
        MetroTrack metroTrack = new MetroTrack( name, tags, sequence );
        if ( trackMode != null ) {
            metroTrack.setTrackMode( trackMode );
        }
        if ( (startSynchronizer != null) && (sequence instanceof MetroSynchronizedStarter) )
            ((MetroSynchronizedStarter)sequence).setStartSynchronizer(startSynchronizer);
        if ( (stopSynchronizer != null ) && (sequence instanceof MetroSynchronizedStopper) )
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
     * This constructor should be called only by {@link Metro#putTracks(Collection, MetroTrackSynchronizer, MetroTrackSynchronizer)}
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
        return "(" + MetroTrack.class.getSimpleName() + " name: '" + this.getName() + ")";
    }
    

    /*
     * This value is only for debugging purpose. 
     */
    private final long uniqueID = (int)(Math.random() * Long.MAX_VALUE);
    long getUniqueID() {
        return uniqueID;
    }

    /**
     * @see metro.MetroMidiOutputSignalAnalyzer#getTrackMode()
     */
    public MetroTrackMode getTrackMode() {
        return this.midiAnalyzer.getTrackMode();
    }
    
    /**
     * @param mode
     * @see metro.MetroMidiOutputSignalAnalyzer#setTrackMode(metro.MetroTrackMode)
     */
    public void setTrackMode(MetroTrackMode mode) {
        midiAnalyzer.setTrackMode(mode);
    }
    
    final List<MetroMidiEvent> inputMidiEvents = new ArrayList<>();
    final List<MetroMidiEvent> outputMidiEvents = new ArrayList<>();
    final List<MetroTrack> registeringTracks = new ArrayList<>();
    final List<MetroTrack> removingTracks = new ArrayList<>();
    final List<MetroTrack> unregisteringTracks = new ArrayList<>();
    final MetroMidiOutputSignalAnalyzer midiAnalyzer = new MetroMidiOutputSignalAnalyzer();

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
            metro.manipulateTrack(Arrays.asList(
                MetroTrackManipulatorBasic.unregister(
                    MetroTrackSelectorBasic.constant(this))));
        }
    }
}
