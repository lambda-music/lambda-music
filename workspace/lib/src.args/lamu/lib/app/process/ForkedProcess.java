package lamu.lib.app.process;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.logging.Level;

import lamu.lib.app.ApplicationComponent;
import lamu.lib.log.Logger;
import lamu.lib.procs.InstanceManager;
import lamu.lib.stream.Stream;

public class ForkedProcess implements ApplicationComponent, Stream {
    public static ForkedProcess forkJavaProcess( String canonicalNameOfMainClass, List<String> arguments ) {
        ArrayList<String> fullArguments = new ArrayList<>();
        fullArguments.addAll( ForkedProcessUtil.getJavaArguments( canonicalNameOfMainClass ) );
        fullArguments.addAll( arguments );
        return new ForkedProcess( fullArguments );
    }
    public static ForkedProcess forkProcess( List<String> arguments ) {
        return new ForkedProcess( arguments );
    }

    
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg) { LOGGER.log(Level.INFO, msg); }
    static void logWarn(String msg) { LOGGER.log(Level.WARNING, msg); }

    
    static final InstanceManager<ForkedProcess> manager = InstanceManager.create(new InstanceManager.Processor<ForkedProcess>() {
        public void quit(ForkedProcess o) {
            o.requestQuit();
        }; 
        @Override
        public void kill(ForkedProcess o) {
            o.quitProcess();
        }
        @Override
        public boolean isAlive(ForkedProcess o) {
            return o.isAlive();
        }
        @Override
        public String getName(ForkedProcess o) {
            return String.join( " " , o.arguments );
        }
    });

    
    private ApplicationComponent parent;
    @Override
    public void setParentApplicationComponent(ApplicationComponent parent) {
        this.parent = parent;
    }

    @Override
    public ApplicationComponent getParentApplicationComponent() {
        return parent;
    }

    private List<String> arguments;
    private ForkedProcess( List<String> arguments ) {
        this.arguments = arguments;
    }
    private Process process=null;
    private OutputStream outputStream=null;
    private InputStream inputStream=null;
    private InputStream errorStream=null;
    @Override
    public InputStream getDownwardErrorStream() {
        if ( errorStream == null )
            throw new IllegalStateException();
        return errorStream;
    }
    @Override
    public OutputStream getUpwardStream() {
        if ( outputStream == null )
            throw new IllegalStateException();
        
        return outputStream;
    }
    @Override
    public InputStream getDownwardStream() {
        if ( this.inputStream == null )
            throw new IllegalStateException();
        return inputStream;
    }
    public Process getProcess() {
        return this.process;
    }
    @Override
    public synchronized void processInit() {
        logInfo( "JavaProcess:processInit()" );
        if ( this.process != null ) {
            System.err.println( "duplicate calling" );
            return;
        }
        try {
            logInfo( String.join( " ", this.arguments ) );
            this.process = ForkedProcessUtil.executeProcess( this.arguments );
            this.outputStream = this.process.getOutputStream();
            this.inputStream = this.process.getInputStream();
            this.errorStream = this.process.getErrorStream();

            ForkedProcess.manager.register(this);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    @Override
    public synchronized void processQuit() {
        logInfo( "JavaProcess:processQuit()" );
        if ( this.process == null ) {
            System.err.println( "this object is not initialized." );
            return;
        }
        quitProcess();
    }
    
    public boolean isAlive() {
        return this.process != null && this.process.isAlive();
    }
    public void killProcess() {
        if ( this.process != null ) {
            this.process.destroyForcibly();
        }
    }
    public void quitProcess() {
        if ( this.process != null ) {
            this.process.destroy();
        }
    }
    
    @Override
    public String toString() {
        return "process";
    }
    static void testMethod(String[] args) throws IOException, InterruptedException {
        if ( args.length == 0 ) {
            Process p =  ForkedProcessUtil.executeJavaProcess( ForkedProcess.class.getCanonicalName(), Arrays.asList("hello") );
            System.out.println( ForkedProcessUtil.readOutput( p.getInputStream() ) );
            System.err.println( ForkedProcessUtil.readOutput( p.getErrorStream() ) );
            Thread.sleep( 3000 );
            p.destroy();
            System.out.println( "end" );
        } else {
            System.out.println("YEAH");
            Thread.sleep( 5000 );
            System.out.println("DONE");
            System.err.println("WARN::TEST");
        }
    }

    private static void testClass(String[] args) throws IOException {
        if ( args.length == 0 ) {
            ForkedProcess process = forkJavaProcess( ForkedProcess.class.getCanonicalName(), Arrays.asList( "foo", "bar" ) );
            process.processInit();
            String out =  new BufferedReader( new InputStreamReader( process.getDownwardStream() ) ).readLine();
            String s = new BufferedReader( new InputStreamReader( System.in ) ).readLine();
            process.processQuit();
            System.out.println( s );
            System.out.println( out );
        } else {
            System.out.println( String.join(",", Arrays.asList( args ) ) );
        }
    }

    public static void main(String[] args) throws IOException, InterruptedException {
        testClass( args );
    }

}
