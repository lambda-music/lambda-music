package pulsar.lib.app;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import pulsar.lib.thread.ThreadInitializerCollection;
import pulsar.lib.thread.ThreadInitializerCollectionContainer;

public class ApplicationVessel implements ApplicationComponent, ThreadInitializerCollectionContainer {
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
    
    ThreadInitializerCollection threadInitializerCollection = new ThreadInitializerCollection( "application", null );
    @Override
    public ThreadInitializerCollection getThreadInitializerCollection() {
        return threadInitializerCollection;
    }
    public void addAll( Collection<? extends ApplicationComponent> cs ) {
        components.addAll(cs);
    }
    public <T extends ApplicationComponent> void add( T c ) {
        components.add(c);
    }
    public void remove( Object c ) {
        components.remove( c );
    }
    @Override
    public void requesetInit() {
        this.threadInitializerCollection.initialize();
        for (  ApplicationComponent c : this.components ) {
            c.setParentApplicationComponent( this );
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

