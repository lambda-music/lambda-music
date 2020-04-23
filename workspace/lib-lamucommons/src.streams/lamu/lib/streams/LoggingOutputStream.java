package lamu.lib.streams;

import java.io.IOException;
import java.io.OutputStream;
import java.lang.invoke.MethodHandles;
import java.util.logging.Level;

import lamu.lib.log.Logger;

public abstract class LoggingOutputStream extends OutputStream {
    protected static final Logger LOGGER = Logger.getLogger(MethodHandles.lookup().lookupClass().getName());
    protected static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    protected static void logInfo(String msg) { LOGGER.log(Level.INFO, msg); }
    protected static void logWarn(String msg) { LOGGER.log(Level.WARNING, msg); }
    protected static void logWarn(Throwable e) { LOGGER.log(Level.WARNING, "warning", e); }
    public abstract OutputStream log();
    public abstract OutputStream out();
    public void write(int b) throws IOException {
        out().write(b);
        try {
            log().write(b);
            log().flush();
        } catch ( IOException e ) {
            logError("failed logging",e);
        }
    }
    public void write(byte[] b) throws IOException {
        out().write(b);
        try {
            log().write(b);
            log().flush();
        } catch ( IOException e ) {
            logError("failed logging",e);
        }
    }
    public void write(byte[] b, int off, int len) throws IOException {
        out().write(b, off, len);
        try {
            log().write(b, off, len);
            log().flush();
        } catch ( IOException e ) {
            logError("failed logging",e);
        }
    }
    public void flush() throws IOException {
        out().flush();
        try {
            log().flush();
        } catch ( IOException e ) {
            logError("failed logging",e);
        }
    }
    public void close() throws IOException {
        try {
            out().close();
        } catch ( IOException e ) {
            throw e;
        } finally {
            try {
                log().close();
            } catch ( IOException e ) {
                logError("failed logging",e);
            }
        }
    }
}
