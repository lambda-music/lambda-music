package pulsar;

import java.util.Collection;

import gnu.lists.LList;
import metro.Metro;
import metro.MetroSequence;
import metro.MetroTrack;

public class PulsarTrack extends MetroTrack implements ReadableSchemeSequence {
    public PulsarTrack(Metro metro, Object name, Collection<Object> tags, MetroSequence sequence) {
        super( metro, name, tags, sequence );
    }

    @Override
    public LList readMusic() {
        if ( this.getSequence() instanceof ReadableSchemeSequence ) {
            return ((ReadableSchemeSequence)(this.getSequence())).readMusic();
        } else {
            throw new RuntimeException( "Unsupported sequence object error" );
        }
    }
    
}
