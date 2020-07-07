package lamu.lib.procs;

import java.io.InputStream;
import java.io.OutputStream;
import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;

import lamu.lib.logging.Logger;

/**
 * It seems that if a class has an abstract method, Kawa cannot call the method
 * unless the user explicitly specifies the type of the variable which stores 
 * the instance of the class.  
 *  
 * The purpose of this class is simply wrap the abstract class 
 * by a concrete class in order to avoid that problem.
 * 
 * (Sat, 19 Oct 2019 01:16:47 +0900)
 */
public class PulsarProcessWrapper {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE,   msg, e   ); }
    static void logInfo (String msg             ) { LOGGER.log(Level.INFO,     msg      ); }
    static void logWarn (String msg             ) { LOGGER.log(Level.WARNING,  msg      ); }
    static final InstanceManager manager = InstanceManager.create(new InstanceManager.Processor<PulsarProcessWrapper>() {
        @Override
        public void kill(PulsarProcessWrapper o) {
            o.destroyForcibly();
        }
        @Override
        public void quit(PulsarProcessWrapper o) {
            o.destroy();
        }
        @Override
        public boolean isAlive(PulsarProcessWrapper o) {
            return o.isAlive();
        }
        public String getName(PulsarProcessWrapper o) {
            return String.join( " " , o.arguments );
        } 
    });
    Process p;
    ArrayList<String> arguments;
    public PulsarProcessWrapper( Process p, ArrayList<String> arguments ) {
        super();
        this.p = p;
        this.arguments = arguments;
        manager.register(this);
    }
    public OutputStream getOutputStream() {
        return p.getOutputStream();
    }
    public InputStream getInputStream() {
        return p.getInputStream();
    }
    public InputStream getErrorStream() {
        return p.getErrorStream();
    }
    public int waitFor() throws InterruptedException {
        return p.waitFor();
    }
    public boolean waitFor(long timeout, TimeUnit unit) throws InterruptedException {
        return p.waitFor( timeout, unit );
    }
    public int exitValue() {
        return p.exitValue();
    }
    public void destroy() {
        p.destroy();
    }
    public Process destroyForcibly() {
        return p.destroyForcibly();
    }
    public boolean isAlive() {
        return p.isAlive();
    }
}
