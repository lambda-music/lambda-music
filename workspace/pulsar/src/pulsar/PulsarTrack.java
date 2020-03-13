package pulsar;

import java.util.Collection;

import gnu.lists.LList;
import lamu.lib.secretary.Invokable;
import metro.Metro;
import metro.MetroSequence;
import metro.MetroTrack;

public class PulsarTrack extends MetroTrack implements SchemeSequenceReadable, Invokable {
    public PulsarTrack(Metro metro, Object name, Collection<Object> tags, MetroSequence sequence) {
        super( metro, name, tags, sequence );
    }
    

    @Override
    public LList readMusic() {
        if ( this.getSequence() instanceof SchemeSequenceReadable ) {
            return ((SchemeSequenceReadable)(this.getSequence())).readMusic();
        } else {
            throw new RuntimeException( "Unsupported sequence object error" );
        }
    }

    @Override
    public Object invoke(Object... args) {
        MetroSequence sequence = getSequence();
        if ( sequence instanceof Invokable ) {
            return ((Invokable)sequence).invoke( args );
        } else {
            throw new IllegalStateException( "the sequence is not an invokable object." );
        }
    }
    
}
