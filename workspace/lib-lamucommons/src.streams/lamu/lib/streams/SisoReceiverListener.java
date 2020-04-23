package lamu.lib.streams;

public interface SisoReceiverListener {
    void process( SisoReceiver receiver, String s );
}