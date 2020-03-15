package lamu.lib.scheme.repl;

import java.io.IOException;
import java.io.PrintStream;

interface SisoMessage {
    void process( SisoReceiver server, PrintStream out ) throws IOException;
}