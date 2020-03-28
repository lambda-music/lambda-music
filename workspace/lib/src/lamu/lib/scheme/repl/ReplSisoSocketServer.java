package lamu.lib.scheme.repl;

import java.net.ServerSocket;
import java.net.Socket;

import lamu.lib.scheme.SchemeEngine;
import lamu.lib.stream.JavaSocketStream;
import lamu.lib.stream.SisoReceiver;
import lamu.lib.stream.SisoSocketServer;

/**
 * Note that this class is not fully implemented.
 */
public class ReplSisoSocketServer extends SisoSocketServer {
    SchemeEngine schemeEngine;
    public ReplSisoSocketServer( SchemeEngine schemeEngine, ServerSocket serverSocket, String threadName ) {
        super( serverSocket, threadName);
        this.schemeEngine = schemeEngine;
    }

    @Override
    protected void accept( Socket socket ) {
        new SisoReceiver( 
            new JavaSocketStream(socket),
            new ReplServer( schemeEngine) ).requestInit();
    }
}
