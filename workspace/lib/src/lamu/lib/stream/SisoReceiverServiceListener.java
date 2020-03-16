package lamu.lib.stream;

public interface SisoReceiverServiceListener extends SisoReceiverListener {
    public void start(SisoReceiver receiver);
    public void end(SisoReceiver receiver);
}
