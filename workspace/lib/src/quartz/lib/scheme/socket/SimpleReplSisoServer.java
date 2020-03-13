package quartz.lib.scheme.socket;

import quartz.lib.scheme.socket.SisoReceiver.SisoListener;

public class SimpleReplSisoServer implements SisoListener {
	@Override
	public void process( SisoReceiver server, String s ) {
		if ("quit".equals(s) || "bye".equals(s)) {
			server.postMessage( SisoReceiver.createPrintMessage("ok"));
			server.postMessage( SisoReceiver.createQuitMessage() );
		} else if ("alive?".equals(s)) {
			server.postMessage( SisoReceiver.createPrintMessage("yes"));
		} else if ("hello".equals(s)) {
			server.postMessage( SisoReceiver.createPrintMessage("hello"));
		} else {
			server.postMessage( SisoReceiver.createPrintMessage("unknown-command"));
		}
	}
	public static void main(String[] args) {
		new SisoReceiver( null, System.in, System.out, new SimpleReplSisoServer() ).requestInit();
	}
}