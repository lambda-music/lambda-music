package lamu.lib.streams;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.invoke.MethodHandles;
import java.util.logging.Level;

import lamu.lib.logging.Logger;

public abstract class LoggingInputStream extends InputStream {
    protected static final Logger LOGGER = Logger.getLogger(MethodHandles.lookup().lookupClass().getName());
    protected static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    protected static void logInfo(String msg) { LOGGER.log(Level.INFO, msg); }
    protected static void logWarn(String msg) { LOGGER.log(Level.WARNING, msg); }
    protected static void logWarn(Throwable e) { LOGGER.log(Level.WARNING, "warning", e); }
    public abstract InputStream in();
    public abstract OutputStream log();

    public int read() throws IOException {
        int b = in().read();
        try {
            log().write(b);
            log().flush();
        } catch ( IOException e ) {
            logError( "failed logging",e);
        }
        return b;
    }
    public int read(byte[] b) throws IOException {
        int s = in().read(b);
        try {
            log().write(b,0,s);
            log().flush();
        } catch ( IOException e ) {
            logError( "failed logging",e);
        }
        return s;
    }
    public int read(byte[] b, int off, int len) throws IOException {
        int s = in().read(b, off, len);
        try {
            log().write(b,off,s);
            log().flush();
        } catch ( IOException e ) {
            logError( "failed logging",e);
        }
        return s;
    }
    public long skip(long n) throws IOException {
        long s = in().skip(n);
        try {
            log().write(new byte[(int)s],0,(int) s);
            log().flush();
        } catch ( IOException e ) {
            logError( "failed logging",e);
        }
        return s;
    }
    public int available() throws IOException {
        return in().available();
    }
    public void close() throws IOException {
        try {
            in().close();
        } finally {
            try {
                log().close();
            } catch ( IOException e ) {
                logError( "failed logging",e);
            }
        }
    }
    public void mark(int readlimit) {
        in().mark(readlimit);
        try {
            log().write( "\n(marked)\n".getBytes() );
            log().flush();
        } catch ( IOException e ) {
            logError( "failed logging",e);
        }
    }
    public void reset() throws IOException {
        in().reset();
        try {
            log().write( "\n(reset)\n".getBytes() );
            log().flush();
        } catch ( IOException e ) {
            logError( "failed logging",e);
        }
    }
    public boolean markSupported() {
        return in().markSupported();
    }
    
}
