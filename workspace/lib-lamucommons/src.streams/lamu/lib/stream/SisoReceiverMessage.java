package lamu.lib.stream;

import java.io.IOException;
import java.io.PrintStream;

public interface SisoReceiverMessage {
    void process( SisoReceiver server, PrintStream out ) throws IOException;
}