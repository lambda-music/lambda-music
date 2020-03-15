package lamu.lib.scheme.repl;

public class SimpleReplListener implements SisoProcessor, SisoListener {
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
    public void start(SisoReceiver receiver) {
    }
    @Override
    public void end(SisoReceiver server) {
    }

    public static void main(String[] args) {
        new SisoReceiver( null, System.in, System.out, new SimpleReplListener() ).requestInit();
    }
}