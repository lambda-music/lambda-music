package lamu.lib.stream;

import java.io.InputStream;
import java.io.OutputStream;

/**
 * This interface manages the standard input/output streams as a metaphor of elevators.
 * In this metaphor, servers are upside and clients are downside.  See {@link ClientsideStream}. 
 */
public interface ServersideStream {
    public InputStream getDownwardStream();
    public InputStream getDownwardErrorStream();
    public OutputStream getUpwardStream();
}
