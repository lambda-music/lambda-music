package lamu.lib.stream;

import java.io.InputStream;
import java.io.OutputStream;

/**
 * This interface manages the standard input/output streams as a metaphor of elevators.
 * In this metaphor, servers are upside and clients are downside.  See {@link ClientsideStream}.
 */
public class ClientsideStreamStdio implements ClientsideStream {
    @Override
    public InputStream getUpwardStream() {
        return System.in;
    }
    @Override
    public OutputStream getDownwardErrorStream() {
        return System.err;
    }
    @Override
    public OutputStream getDownwardStream() {
        return System.out;
    }
}
