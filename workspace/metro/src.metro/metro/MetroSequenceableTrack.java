package metro;

import java.util.Collection;

import lamu.lib.Invokable;

public class MetroSequenceableTrack extends MetroBufferedTrack implements MetroSequenceable, MetroReadable, Invokable {
    private final MetroSequence sequence;
    @Override
    public MetroSequence getSequence() {
        return this.sequence;
    }
    
    public MetroSequenceableTrack(Object name, Collection<Object> tags,
        MetroSyncType syncType, MetroSyncTrack syncTrack, double syncOffset,
        MetroSequence sequence ) {
        super(name, tags, syncType, syncTrack, syncOffset);
        this.sequence = sequence;
    }
    @Override
    public <T> void processBuffered(Metro metro, MetroBufferedMidiReceiver<T> buffer) {
        this.sequence.processBuffered(metro, this, buffer);
    }

    @Override
    public Object readContent() {
        MetroSequence seq = this.getSequence();
        if ( seq instanceof MetroReadable ) {
            return ((MetroReadable)seq).readContent();
        } else {
            throw new RuntimeException( "the current sequence is not readable" );
        }
    }

    @Override
    public Object invoke(Object... args) {
        MetroSequence sequence = getSequence();
        if ( sequence instanceof Invokable ) {
            return ((Invokable)sequence).invoke( args );
        } else {
            throw new IllegalStateException( "the current sequence is not an invokable object." );
        }
    }

    /**
     * This is a utility method to create a Metro instance with a single sequence.
     * 
     * @param clientName
     * @param sequence
     * @return
     * @throws MetroException
     */
    public static Metro startClient( String clientName, MetroSequence sequence ) throws MetroException {
        try {
            Metro metro = new Metro();
            metro.open( clientName );
            synchronized ( metro.getMetroLock() ) {
                try {
                    metro.registerTrack( create( "main", null, MetroSyncType.IMMEDIATE,null,0.0d, sequence ) );
                } finally {
                    metro.notifyTrackChange();
                }
            }
            return metro;
        } catch (MetroException ex) {
            Metro.logError( null, ex);
            throw ex;
        }
    }

    /**
     * A factory method to create an instance of {@link MetroBufferedTrack}.
     *  
     * @param name
     * @param tags
     * @param sequence
     * @return
     */
    public static MetroSequenceableTrack create(Object name, Collection<Object> tags,
        MetroSyncType syncType, MetroSyncTrack syncTrack, double syncOffset,
        MetroSequence sequence) {
        return new MetroSequenceableTrack(name, tags, syncType,syncTrack,syncOffset,sequence);
    }
}
