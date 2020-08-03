package metro;

import java.util.Collection;

class DefaultMetroTrackFactory implements MetroTrackFactory {
    private Object name;
    private Collection<Object> tags;
    private MetroSequenceFactory sequenceFactory;
    private MetroTrackMode trackMode;
    private MetroTrackSynchronizer startSynchronizer;
    private MetroTrackSynchronizer stopSynchronizer;
    
    /**
     * Create a track factory object. For further information, see
     * {@link MetroTrack#create(Object, Collection, MetroSequence, MetroTrackMode, MetroTrackSynchronizer, MetroTrackSynchronizer)}.
     * 
     * @param name              
     * @param tags              
     * @param sequenceFactory
     * @param trackMode 
     * @param startSynchronizer
     * @param stopSynchronizer
     */
    DefaultMetroTrackFactory(
        Object name, 
        Collection<Object> tags, 
        MetroSequenceFactory sequenceFactory,
        MetroTrackMode trackMode, 
        MetroTrackSynchronizer startSynchronizer, MetroTrackSynchronizer stopSynchronizer )
    {
        super();
        this.name              = name;
        this.tags              = tags;
        this.sequenceFactory   = sequenceFactory;
        this.trackMode         = trackMode;
        this.startSynchronizer = startSynchronizer;
        this.stopSynchronizer  = stopSynchronizer;
    }

    @Override
    public MetroTrack createTrack() {
        return MetroTrack.create( 
            name, 
            tags,
            sequenceFactory.createSequence(),
            trackMode,
            startSynchronizer, 
            stopSynchronizer );
    }
    @Override
    public String toString() {
        return String.format( "(track-factory name: %s tags: %s sequence: %s)" , 
            name, 
            MetroUtils.listToString(tags), 
            sequenceFactory )  ;
    }
}

/**
 * This class is also known as "Newt" object in which generates a MetroTrack
 * object each time {@link #createTrack()} method is called.
 * 
 * @author ats
 */
public interface MetroTrackFactory {
    public static MetroTrackFactory createDefault(
        Object name, 
        Collection<Object> tags, 
        MetroSequenceFactory sequenceFactory,
        MetroTrackMode trackMode, 
        MetroTrackSynchronizer startSynchronizer,
        MetroTrackSynchronizer stopSynchronizer ) 
    {
        return new DefaultMetroTrackFactory( name, tags, sequenceFactory, trackMode, startSynchronizer, stopSynchronizer );
    }

    
    /**
     * Create a track factory. The classes which implements this interface should
     * define own factory method to return an instance of MetroTrack.
     * 
     * @return
     *    an instanciated MetroTrack object
     */
    MetroTrack createTrack();
}
