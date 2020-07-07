package lamu.lib.logging;

import java.util.logging.Level;

public class PlainLogger {
    String format = "[%10s] [%-5s] %s";
    public String getFormat() {
        return format;
    }
    public void setFormat(String format) {
        this.format = format;
    }
    public static PlainLogger getLogger(String name) {
        return new PlainLogger(name);
    }
    private String name;
    private PlainLogger( String name ) {
        this.name = name;
    }
    public void log( Level level, String msg, Throwable e ) {
        System.err.println( String.format( format, name, level, msg ) );
        e.printStackTrace();
        System.err.flush();
    }
    public void log( Level level, String msg ) {
        System.err.println( String.format( format, name, level, msg ) );
        System.err.flush();
    }
}
