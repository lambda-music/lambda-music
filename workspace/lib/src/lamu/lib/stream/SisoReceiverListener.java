package lamu.lib.stream;

public interface SisoReceiverListener {
    void process( SisoReceiver receiver, String s );
}