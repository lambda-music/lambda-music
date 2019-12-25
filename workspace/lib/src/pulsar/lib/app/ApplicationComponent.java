package pulsar.lib.app;

public interface ApplicationComponent {
    void setParentApplicationComponent( ApplicationComponent parent );
    ApplicationComponent getParentApplicationComponent();
    void processInit();
    void processQuit();
}
