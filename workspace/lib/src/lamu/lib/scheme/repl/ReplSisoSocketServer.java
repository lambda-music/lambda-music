package lamu.lib.scheme.repl;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;

import lamu.lib.scheme.SchemeEngine;
import lamu.lib.stream.SisoReceiver;
import lamu.lib.stream.SisoSocketServer;

public class ReplSisoSocketServer extends SisoSocketServer {
    SchemeEngine schemeEngine;
    public ReplSisoSocketServer( SchemeEngine schemeEngine, ServerSocket serverSocket, String threadName ) {
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
                    new ReplServer( schemeEngine) );
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
