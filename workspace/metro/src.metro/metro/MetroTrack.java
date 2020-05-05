package metro;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

public abstract class MetroTrack {
    /**
     * Note that the String object which is stored in name field must be interned.  
     */
    private final Object name;
    private final Collection<Object> tags;
    public Object getName() {
        return name;
    }
    public Collection<Object> getTags() {
        return tags;
    }
    private static transient int uniqueTrackNameCounter = 0;
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
    private final int uniqueID = (int) (Math.random()* Integer.MAX_VALUE);
    int getUniqueID() {
        return uniqueID;
    }

    // This method was formerly checkBuffer()
    public abstract void processBuffer(Metro metro, int barLengthInFrames) throws MetroException;
    
    public abstract void progressCursor(Metro metro, int nframes, 
        List<MetroMidiEvent> inputMidiEventList, List<MetroMidiEvent> outputMidiEventList ) throws MetroException;

    // Moved from MetroSyncTrack (Tue, 05 May 2020 16:47:27 +0900)
    /**
     * Removes this track from the parent Metro object. Calling this method has no
     * effect if this track does not belong to the specified Metro object, This
     * method calls {@link Metro#unregisterTrack(Collection)} method to remove this
     * track. This behavior is a default implementation. Any subclasses which
     * inherit this class should properly reimplement their desirable behavior.
     * 
     * @param metro
     *    The parent Metro object.
     */
    public void removeGracefully(Metro metro) {
        metro.unregisterTrack( this );
    }
}
