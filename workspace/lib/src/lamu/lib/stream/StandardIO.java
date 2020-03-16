package lamu.lib.stream;

import java.io.InputStream;
import java.io.OutputStream;

public interface StandardIO {
    public InputStream getInputStream();
    public InputStream getErrorStream();
    public OutputStream getOutputStream();
}
