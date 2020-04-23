package lamu.lib.streams;

import java.io.IOException;
import java.io.OutputStream;

public class NullOutputStream extends OutputStream {
    public static final OutputStream INSTANCE = new NullOutputStream();
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
