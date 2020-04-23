package lamu.lib.apps;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

public class ApplicationVessel implements ApplicationComponent {
    final String name;
    public ApplicationVessel(String name) {
        super();
        this.name = name;
    }
    @Override
    public String toString() {
        return "[" + getClass().getSimpleName() +":"+  this.name + "]";
    }

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
    
    public void addAll( Collection<? extends ApplicationComponent> cs ) {
        for ( ApplicationComponent c : cs ) {
            this.add( c );
        }
    }
    public <T extends ApplicationComponent> void add( T c ) {
        c.setParentApplicationComponent( this );
        if ( components.contains(c))
            throw new IllegalArgumentException( "this container already has the specified element " + c );
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

