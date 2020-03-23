package lamu.lib.stream;

import java.io.InputStream;
import java.io.OutputStream;

public class StdioStream implements Stream {
    public static final StdioStream INSTANCE = new StdioStream();
    @Override
    public InputStream getDownwardStream() {
        return System.in;
    }

    @Override
    public InputStream getDownwardErrorStream() {
        return NullInputStream.INSTANCE;
    }

    @Override
    public OutputStream getUpwardStream() {
        return System.out;
    }

}
