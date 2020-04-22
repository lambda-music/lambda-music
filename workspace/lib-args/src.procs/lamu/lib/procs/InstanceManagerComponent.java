package lamu.lib.procs;

import lamu.lib.app.ApplicationComponent;

public final class InstanceManagerComponent implements ApplicationComponent {
    @Override
    public void processInit() {
    }

    @Override
    public void processQuit() {
        InstanceManager.shutdown();
    }

    ApplicationComponent parent= null;
    @Override
    public void setParentApplicationComponent(ApplicationComponent parent) {
        this.parent = parent;
    }
    @Override
    public ApplicationComponent getParentApplicationComponent() {
        return parent;
    }
}