package pulsar.lib.log;

import java.util.logging.Level;
import java.util.logging.Logger;

public class PulsarLogger {
    public static SimpleConsole console=null;
    public static synchronized SimpleConsole getConsole() {
        if ( console == null ) {
            console = new SimpleConsole();
        }

        return console;
    }
    private Logger logger;
    public PulsarLogger( String name ) {
        this.logger = Logger.getLogger( name );
    }
    public static PulsarLogger getLogger(String name) {
        return new PulsarLogger( name );
    }
    public void log(Level l, String msg) {
        this.logger.log( l, msg );
    }
    public void log(Level l, String msg, Throwable e) {
        this.logger.log( l, msg, e );
        
        SimpleConsole console = getConsole();
        console.addText( e );
    }
}
