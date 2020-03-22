package lamu.lib.stream;

import lamu.lib.app.ApplicationComponent;

public class DefaultStreamConnector implements ApplicationComponent {
    public static ApplicationComponent connect( ServersideStream from, ClientsideStream to ) {
        return new DefaultStreamConnector( from, to );
    }
    public static void connect( ClientsideStream from, ServersideStream to ) {
    }
    
    private final ClientsideStream clientsideStream;
    private final ServersideStream serversideStream;
    private boolean initDone = false;
    private StreamPump downwardPump;
    private StreamPump downwardErrorPump;
    private StreamPump upwardPump;
    public DefaultStreamConnector( ServersideStream serversideStream, ClientsideStream clientsideStream) {
        super();
        this.clientsideStream = clientsideStream;
        this.serversideStream = serversideStream;
    }
    ApplicationComponent parent;
    @Override
    public ApplicationComponent getParentApplicationComponent() {
        return parent;
    }
    @Override
    public void setParentApplicationComponent(ApplicationComponent parent) {
        this.parent = parent;
    }
    
    @Override
    public synchronized void processInit() {
        if ( initDone ) {
            return;
        }
        initDone = true;
        upwardPump = new StreamPump( clientsideStream.getUpwardStream(), serversideStream.getUpwardStream() );
        downwardPump = new StreamPump( serversideStream.getDownwardStream(), clientsideStream.getDownwardStream() );
        downwardErrorPump = new StreamPump( serversideStream.getDownwardErrorStream(), clientsideStream.getDownwardErrorStream() );
        
        upwardPump.requestInit();
        downwardPump.requestInit();
        downwardErrorPump.requestInit();
    }
    @Override
    public synchronized void processQuit() {
        if ( ! initDone )
            return;
        upwardPump.requestQuit();
        downwardPump.requestQuit();
        downwardErrorPump.requestQuit();
    }
}
