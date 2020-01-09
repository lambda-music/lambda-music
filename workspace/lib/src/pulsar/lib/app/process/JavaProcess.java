package pulsar.lib.app.process;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.lang.invoke.MethodHandles;
import java.lang.management.ManagementFactory;
import java.lang.management.RuntimeMXBean;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.logging.Level;

import pulsar.lib.app.ApplicationComponent;
import pulsar.lib.log.PulsarLogger;

public class JavaProcess implements ApplicationComponent{
    static final PulsarLogger LOGGER = PulsarLogger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) {
        LOGGER.log(Level.SEVERE, msg, e);
    }
    static void logInfo(String msg) {
        LOGGER.log(Level.INFO, msg);
    }
    static void logWarn(String msg) {
        LOGGER.log(Level.WARNING, msg);
    }

    private ApplicationComponent parent;
    @Override
    public void setParentApplicationComponent(ApplicationComponent parent) {
        this.parent = parent;
    }

    @Override
    public ApplicationComponent getParentApplicationComponent() {
        return parent;
    }

    private String canonicalNameOfMainClass;
    private List<String> arguments;
    public JavaProcess( String canonicalNameOfMainClass, List<String> arguments ) {
        this.canonicalNameOfMainClass = canonicalNameOfMainClass;
        this.arguments = arguments;
    }
    
    static String readOutput( InputStream i ) throws IOException{
        BufferedReader reader = 
                new BufferedReader(new InputStreamReader(i));
        StringBuilder builder = new StringBuilder();
        String line = null;
        while ( (line = reader.readLine()) != null) {
            builder.append(line);
            builder.append(System.getProperty("line.separator"));
        }
        String result = builder.toString();
        return result;
    }
    static Process executeJavaProcess( String canonicalNameOfMainClass, List<String> arguments ) throws IOException{
        // https://stackoverflow.com/questions/1490869/how-to-get-vm-arguments-from-inside-of-java-application
        ArrayList<String> fullArguments = new ArrayList<>();
        RuntimeMXBean r = ManagementFactory.getRuntimeMXBean();
        fullArguments.add( "java" );
        fullArguments.addAll( r.getInputArguments() );
        fullArguments.add("-classpath");
        fullArguments.add( r.getClassPath() );
        fullArguments.add( canonicalNameOfMainClass );
        fullArguments.addAll( arguments );
        System.out.println( fullArguments );
        ProcessBuilder b = new ProcessBuilder( fullArguments );
        b.inheritIO();
        
        return b.start();
    }
    static void testMethod(String[] args) throws IOException, InterruptedException {
        if ( args.length == 0 ) {
            Process p =  executeJavaProcess( JavaProcess.class.getCanonicalName(), Arrays.asList("hello"));
            System.out.println( readOutput( p.getInputStream() ) );
            System.err.println( readOutput( p.getErrorStream() ) );
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
            JavaProcess process = new JavaProcess( 
                JavaProcess.class.getCanonicalName(),
                Arrays.asList( "foo", "bar" ) );
            process.processInit();
            String out =  new BufferedReader( new InputStreamReader( process.getInputStream() ) ).readLine();
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

    private Process process=null;
    private OutputStream outputStream=null;
    private InputStream inputStream=null;
    private InputStream errorStream=null;
    public InputStream getErrorStream() {
        return errorStream;
    }
    public OutputStream getOutputStream() {
        return outputStream;
    }
    public InputStream getInputStream() {
        return inputStream;
    }
    public Process getProcess() {
        return process;
    }
    @Override
    public synchronized void processInit() {
        logInfo( "JavaProcess:processInit()" );
        if ( process != null ) {
            System.err.println( "duplicate calling" );
            return;
        }
        try {
            logInfo( String.join( " ", arguments ) );
            this.process = executeJavaProcess( canonicalNameOfMainClass, arguments );
            this.outputStream = this.process.getOutputStream();
            this.inputStream = this.process.getInputStream();
            this.errorStream = this.process.getErrorStream();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    @Override
    public synchronized void processQuit() {
        logInfo( "JavaProcess:processQuit()" );
        if ( process == null ) {
            System.err.println( "this object is not initialized." );
            return;
        }
        this.process.destroyForcibly();
    }
    
}
