package quartz.lib.scheme.socket;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;

import quartz.lib.app.ApplicationComponent;
import quartz.lib.thread.ThreadInitializerCollection;
import quartz.lib.thread.ThreadInitializerCollectionContainer;

public abstract class SisoServer implements 
		ThreadInitializerCollectionContainer, ApplicationComponent , Runnable  
{
	ServerSocket serverSocket;
	Thread thread;
	public SisoServer(ServerSocket serverSocket, String threadName ) {
		super();
		this.serverSocket = serverSocket;
		this.thread = new Thread( this, "SisoServer" + threadName );
	}

	@Override
	public void run() {
		try {
			for (;;) {
				accept(this.serverSocket.accept());
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	protected abstract void accept(Socket socket);
	
	@Override
	public void processInit() {
		this.thread.start();
	}
	@Override
	public void processQuit() {
		try {
			serverSocket.close();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
    private ThreadInitializerCollection threadInitializerCollection = new ThreadInitializerCollection( "siso-server", this );
    public ThreadInitializerCollection getThreadInitializerCollection() {
        return threadInitializerCollection;
    }
    private ApplicationComponent parentApplicationComponent;
    @Override
    public ApplicationComponent getParentApplicationComponent() {
        return this.parentApplicationComponent;
    }
    @Override
    public void setParentApplicationComponent(ApplicationComponent parentApplicationComponent) {
        this.parentApplicationComponent = parentApplicationComponent;
    }
    
    
}
