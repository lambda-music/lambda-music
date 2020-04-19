package lamu.lib.scheme.repl;

import java.net.ServerSocket;
import java.net.Socket;

import lamu.lib.scheme.MultiplexEvaluator;
import lamu.lib.stream.JavaSocketStream;
import lamu.lib.stream.SisoReceiver;
import lamu.lib.stream.SisoSocketServer;

/**
 * Note that this class is not fully implemented. (Sun, 29 Mar 2020 04:13:01 +0900)
 */
public class ReplSisoSocketServer extends SisoSocketServer {
    MultiplexEvaluator multiplexEvaluator;
    public ReplSisoSocketServer( MultiplexEvaluator multiplexEvaluator, ServerSocket serverSocket, String threadName ) {
        super( serverSocket, threadName);
        this.multiplexEvaluator = multiplexEvaluator;
    }

    @Override
    protected void accept( Socket socket ) {
        new SisoReceiver( 
            new JavaSocketStream(socket),
            new ReplServer( multiplexEvaluator) ).requestInit();
    }
}
