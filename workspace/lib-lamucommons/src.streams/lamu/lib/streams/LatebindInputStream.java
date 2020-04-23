package lamu.lib.streams;

import java.io.IOException;
import java.io.InputStream;
import java.lang.invoke.MethodHandles;
import java.util.logging.Level;

import lamu.lib.log.Logger;

public abstract class LatebindInputStream extends InputStream {
    protected static final Logger LOGGER = Logger.getLogger(MethodHandles.lookup().lookupClass().getName());
    protected static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    protected static void logInfo(String msg) { LOGGER.log(Level.INFO, msg); }
    protected static void logWarn(String msg) { LOGGER.log(Level.WARNING, msg); }
    protected static void logWarn(Throwable e) { LOGGER.log(Level.WARNING, "warning", e); }
    public abstract InputStream in();

    public int read() throws IOException {
        int b = in().read();
        return b;
    }
    public int read(byte[] b) throws IOException {
        int s = in().read(b);
        return s;
    }
    public int read(byte[] b, int off, int len) throws IOException {
        int s = in().read(b, off, len);
        return s;
    }
    public long skip(long n) throws IOException {
        long s = in().skip(n);
        return s;
    }
    public int available() throws IOException {
        return in().available();
    }
    public void close() throws IOException {
        in().close();
    }
    public void mark(int readlimit) {
        in().mark(readlimit);
    }
    public void reset() throws IOException {
        in().reset();
    }
    public boolean markSupported() {
        return in().markSupported();
    }
}
