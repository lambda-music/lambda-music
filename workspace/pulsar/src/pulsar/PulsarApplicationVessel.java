package pulsar;

import java.util.ArrayDeque;
import java.util.Collection;
import java.util.Deque;

public class PulsarApplicationVessel implements PulsarApplicationComponent {
    private Deque<PulsarApplicationComponent> components;
    public PulsarApplicationVessel( Collection<PulsarApplicationComponent> components ) {
        this.components = new ArrayDeque<>( components );
    }
    @Override
    public void requesetInit() {
        for (  PulsarApplicationComponent c : this.components ) {
            c.requesetInit();
        }
    }
    @Override
    public void requestShutdown() {
        for (  PulsarApplicationComponent c : this.components ) {
            c.requestShutdown();
        }
    }
}

