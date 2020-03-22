package lamu.lib.stream;

import java.io.InputStream;
import java.io.OutputStream;

public interface ClientsideStream {
    public InputStream getUpwardStream();
    public OutputStream getDownwardErrorStream();
    public OutputStream getDownwardStream();
}
