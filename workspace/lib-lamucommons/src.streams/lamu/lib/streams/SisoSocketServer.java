package lamu.lib.streams;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;

import lamu.lib.apps.ApplicationComponent;

public abstract class SisoSocketServer implements ApplicationComponent , Runnable {
    ServerSocket serverSocket;
    Thread thread;
    public SisoSocketServer(ServerSocket serverSocket, String threadName ) {
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
