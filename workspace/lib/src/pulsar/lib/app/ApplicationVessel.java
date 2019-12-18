package pulsar.lib.app;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class ApplicationVessel implements ApplicationComponent {
    private List<ApplicationComponent> components = new ArrayList<>();
    private ApplicationComponent parentApplicationComponent;
    @Override
    public ApplicationComponent getParentApplicationComponent() {
        return this.parentApplicationComponent;
    }
    @Override
    public void setParentApplicationComponent(ApplicationComponent parentApplicationComponent) {
        this.parentApplicationComponent = parentApplicationComponent;
    }
    public void addAll( Collection<? extends ApplicationComponent> cs ) {
        components.addAll(cs);
    }
    public <T extends ApplicationComponent> void add( T c ) {
        components.add(c);
    }
    @Override
    public void requesetInit() {
        for (  ApplicationComponent c : this.components ) {
            c.requesetInit();
        }
    }
    @Override
    public void requestShutdown() {
        for (  ApplicationComponent c : this.components ) {
            c.requestShutdown();
        }
    }
}

