package lamu.lib.stream;

import java.io.IOException;
import java.io.OutputStream;
import java.lang.invoke.MethodHandles;
import java.util.logging.Level;

import lamu.lib.log.Logger;

public abstract class LatebindOutputStream extends OutputStream {
    protected static final Logger LOGGER = Logger.getLogger(MethodHandles.lookup().lookupClass().getName());
    protected static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    protected static void logInfo(String msg) { LOGGER.log(Level.INFO, msg); }
    protected static void logWarn(String msg) { LOGGER.log(Level.WARNING, msg); }
    protected static void logWarn(Throwable e) { LOGGER.log(Level.WARNING, "warning", e); }
    public abstract OutputStream out();
    public void write(int b) throws IOException {
        out().write(b);
    }
    public void write(byte[] b) throws IOException {
        out().write(b);
    }
    public void write(byte[] b, int off, int len) throws IOException {
        out().write(b, off, len);
    }
    public void flush() throws IOException {
        out().flush();
    }
    public void close() throws IOException {
        out().close();
    }
}
