package lamu.lib.scheme.repl;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;

import lamu.lib.scheme.SchemeEngine;

public class ReplSisoServer extends SisoServer {
    SchemeEngine schemeEngine;
    public ReplSisoServer( SchemeEngine schemeEngine, ServerSocket serverSocket, String threadName ) {
        super( serverSocket, threadName);
        this.schemeEngine = schemeEngine;
    }

    @Override
    protected void accept( Socket socket ) {
        try {
            new SisoReceiver( 
                    socket, 
                    socket.getInputStream(), 
                    socket.getOutputStream(),
                    new ReplSisoListener( schemeEngine ) );
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
