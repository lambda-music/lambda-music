package lamu.lib.streams;

import java.io.InputStream;
import java.io.OutputStream;

public class NullStream implements Stream {
    public static final NullStream INSTANCE = new NullStream();
    @Override
    public InputStream getDownwardStream() {
        return NullInputStream.INSTANCE;
    }

    @Override
    public InputStream getDownwardErrorStream() {
        return NullInputStream.INSTANCE;
    }

    @Override
    public OutputStream getUpwardStream() {
        return NullOutputStream.INSTANCE;
    }
    @Override
    public String toString() {
        return "nullio";
    }
}

