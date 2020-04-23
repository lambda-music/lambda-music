package lamu.lib.evaluators.repl;

import java.net.ServerSocket;
import java.net.Socket;

import lamu.lib.evaluators.MultiplexEvaluator;
import lamu.lib.evaluators.ThreadManager;
import lamu.lib.streams.JavaSocketStream;
import lamu.lib.streams.SisoReceiver;
import lamu.lib.streams.SisoSocketServer;

/**
 * Note that this class is not fully implemented. (Sun, 29 Mar 2020 04:13:01 +0900)
 */
public class ReplSisoSocketServer extends SisoSocketServer {
    final ThreadManager threadManager;
    final MultiplexEvaluator multiplexEvaluator;
    public ReplSisoSocketServer( ThreadManager threadManager, MultiplexEvaluator multiplexEvaluator, ServerSocket serverSocket, String threadName ) {
        super( serverSocket, threadName);
        this.threadManager = threadManager;
        this.multiplexEvaluator = multiplexEvaluator;
    }

    @Override
    protected void accept( Socket socket ) {
        new SisoReceiver( 
            new JavaSocketStream(socket),
            new ReplServer( threadManager, multiplexEvaluator ) ).requestInit();
    }
}
