package lamu.lib.app;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import lamu.lib.thread.ThreadInitializerCollection;
import lamu.lib.thread.ThreadInitializerCollectionContainer;

public class ApplicationVessel implements ApplicationComponent, ThreadInitializerCollectionContainer {
    private List<ApplicationComponent> components = new ArrayList<>();
    private ApplicationComponent parent;
    @Override
    public ApplicationComponent getParentApplicationComponent() {
        return this.parent;
    }
    @Override
    public void setParentApplicationComponent(ApplicationComponent parentApplicationComponent) {
        this.parent = parentApplicationComponent;
    }
    
    ThreadInitializerCollection threadInitializerCollection = new ThreadInitializerCollection( "application", null );
    @Override
    public ThreadInitializerCollection getThreadInitializerCollection() {
        return threadInitializerCollection;
    }
    public void addAll( Collection<? extends ApplicationComponent> cs ) {
        for ( ApplicationComponent c : cs ) {
            this.add( c );
        }
    }
    public <T extends ApplicationComponent> void add( T c ) {
        c.setParentApplicationComponent( this );
        components.add(c);
    }
    public void remove( ApplicationComponent c ) {
        if ( components.contains( c ) )
            c.setParentApplicationComponent( null );
        components.remove( c );
    }
    public List<ApplicationComponent> getComponents() {
		return Collections.unmodifiableList( components );
	}
    @Override
    public void processInit() {
        ApplicationComponent.logInfo( "ApplicationVessel:processInit()" );
        for ( ApplicationComponent c : this.components ) {
            c.processInit();
        }
    }
    @Override
    public void processQuit() {
        ApplicationComponent.logInfo( "ApplicationVessel:processQuit()" );
        for (  ApplicationComponent c : this.components ) {
            c.processQuit();
        }
    }
}

