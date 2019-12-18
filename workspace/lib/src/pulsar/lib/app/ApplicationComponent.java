package pulsar.lib.app;

public interface ApplicationComponent {
    void setParentApplicationComponent( ApplicationComponent parent );
    ApplicationComponent getParentApplicationComponent();
    void requesetInit();
    void requestShutdown();
}
