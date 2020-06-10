package metro;

import java.util.Collection;

class DefaultMetroTrackFactory implements MetroTrackFactory {
    private Object name;
    private Collection<Object> tags;
    private MetroSequenceFactory sequenceFactory;
    private MetroTrackSynchronizer startSynchronizer;
    private MetroTrackSynchronizer stopSynchronizer;
    DefaultMetroTrackFactory(
        Object name, Collection<Object> tags, MetroSequenceFactory sequenceFactory,
        MetroTrackSynchronizer startSynchronizer, MetroTrackSynchronizer stopSynchronizer )
    {
        super();
        this.name              = name;
        this.tags              = tags;
        this.sequenceFactory   = sequenceFactory;
        this.startSynchronizer = startSynchronizer;
        this.stopSynchronizer  = stopSynchronizer;
    }
    DefaultMetroTrackFactory(
        Object name, Collection<Object> tags, MetroSequenceFactory sequenceFactory )
    {
        super();
        this.name              = name;
        this.tags              = tags;
        this.sequenceFactory   = sequenceFactory;
        this.startSynchronizer = null;
        this.stopSynchronizer  = null;
    }

    @Override
    public MetroTrack createTrack() {
        return MetroTrack.create( 
            name, 
            tags,
            sequenceFactory.createSequence(),
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
        Object name, Collection<Object> tags, MetroSequenceFactory sequenceFactory,
        MetroTrackSynchronizer startSynchronizer, MetroTrackSynchronizer stopSynchronizer ) 
    {
        return new DefaultMetroTrackFactory( name, tags, sequenceFactory, startSynchronizer, stopSynchronizer );
    }
    public static MetroTrackFactory createDefault(Object name, Collection<Object> tags, MetroSequenceFactory sequenceFactory) {
        return new DefaultMetroTrackFactory( name, tags, sequenceFactory );
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
