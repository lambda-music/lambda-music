package lamu.lib.evaluators.repl;

import lamu.lib.streams.SisoReceiver;
import lamu.lib.streams.SisoReceiverListener;
import lamu.lib.streams.SisoReceiverServiceListener;
import lamu.lib.streams.StdioStream;

public class SimpleReplService implements SisoReceiverListener, SisoReceiverServiceListener {
    @Override
    public void process( SisoReceiver receiver, String s ) {
        if ("quit".equals(s) || "bye".equals(s)) {
            receiver.postMessage( SisoReceiver.createPrintMessage("ok"));
            receiver.postMessage( SisoReceiver.createQuitMessage() );
        } else if ("alive?".equals(s)) {
            receiver.postMessage( SisoReceiver.createPrintMessage("yes"));
        } else if ("hello".equals(s)) {
            receiver.postMessage( SisoReceiver.createPrintMessage("hello"));
        } else {
            receiver.postMessage( SisoReceiver.createPrintMessage("unknown-command"));
        }
    }
    @Override
    public void notifyParent(SisoReceiver receiver) {
    }
   @Override
    public void start(SisoReceiver receiver) {
    }
    @Override
    public void end(SisoReceiver server) {
    }

    public static void main(String[] args) {
        new SisoReceiver( StdioStream.INSTANCE, new SimpleReplService() ).requestInit();
    }
}