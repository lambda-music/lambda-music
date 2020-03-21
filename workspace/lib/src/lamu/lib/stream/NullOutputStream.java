package lamu.lib.stream;

import java.io.IOException;
import java.io.OutputStream;

public class NullOutputStream extends OutputStream {
    public void write(int b) throws IOException {
    }
    public void write(byte[] b) throws IOException {
    }

    public void write(byte[] b, int off, int len) throws IOException {
    }

    public void flush() throws IOException {
    }

    public void close() throws IOException {
    }
}
