package lamu;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.concurrent.TimeUnit;

/**
 * It seems that if a class has an abstract method, Kawa cannot call the method
 * unless its user explicitly specifies the type of the variable which stores 
 * the instance of the class.  
 *  
 * The intension of this class is simply wrap the abstract class 
 * by a concrete class in order to avoid the problem.
 * 
 * (Sat, 19 Oct 2019 01:16:47 +0900)
 */
public class PulsarProcessWrapper {
    Process p;
    public PulsarProcessWrapper(Process p) {
        super();
        this.p = p;
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
