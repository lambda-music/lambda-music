package lamu.lib.stream;

import java.io.Closeable;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.Socket;

public class JavaSocketStream implements Stream, Closeable {
    final Socket socket;
    public JavaSocketStream(Socket socket) {
        super();
        this.socket = socket;
    }
    @Override
    public void close() throws IOException {
        this.socket.close();
    }

    @Override
    public InputStream getDownwardStream() {
        try {
            return this.socket.getInputStream();
        } catch (IOException e) {
            throw new Error(e);
        }
    }

    @Override
    public InputStream getDownwardErrorStream() {
        return NullInputStream.INSTANCE;
    }

    @Override
    public OutputStream getUpwardStream() {
        try {
            return this.socket.getOutputStream();
        } catch (IOException e) {
            throw new Error(e);
        }
    }
}
