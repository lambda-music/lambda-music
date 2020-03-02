package quartz.lib.log;

import java.util.logging.Level;
import java.util.logging.Logger;

public class SimpleConsoleLogger {
    private Logger logger;
    public SimpleConsoleLogger( String name ) {
        this.logger = Logger.getLogger( name );
    }
    public static SimpleConsoleLogger getLogger(String name) {
        return new SimpleConsoleLogger( name );
    }
    public void log(Level l, String msg) {
        this.logger.log( l, msg );
    }
    public void log(Level l, String msg, Throwable e) {
        this.logger.log( l, msg, e );
        SimpleConsole.getConsole().addText( e );
    }
}
