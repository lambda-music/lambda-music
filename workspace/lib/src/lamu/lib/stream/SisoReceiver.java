package lamu.lib.stream;

import java.io.BufferedReader;
import java.io.Closeable;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintStream;
import java.lang.invoke.MethodHandles;
import java.util.Collection;
import java.util.concurrent.BlockingDeque;
import java.util.concurrent.LinkedBlockingDeque;
import java.util.logging.Level;

import lamu.lib.app.ApplicationComponent;
import lamu.lib.log.Logger;

/**
 * SisoServer stands for SImple SOcket Server.
 * 
 */
public class SisoReceiver<T extends SisoReceiverServiceListener> implements ApplicationComponent {
    private static final String SHEBANG = "#!";
    protected static final Logger LOGGER = Logger.getLogger(MethodHandles.lookup().lookupClass().getName());
    protected static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    protected static void logInfo(String msg) { LOGGER.log(Level.INFO, msg); }
    protected static void logWarn(String msg) { LOGGER.log(Level.WARNING, msg); }
    protected static void logWarn(Throwable e) { LOGGER.log(Level.WARNING, "warning", e); }

    // ADDED (Sun, 19 Apr 2020 16:05:02 +0900) This is empty for now.
    protected final Runnable threadInitializer = new Runnable() {
        @Override
        public void run() {
        }
    };
    public Runnable getThreadInitializer() {
        return threadInitializer;
    }
    protected final Stream stream;
    protected final Thread inThread;
    protected final Thread outThread;
    protected InputStream in;
    protected OutputStream out;
    protected BufferedReader i;
    protected PrintStream o;
    protected T listener;
    public SisoReceiver( Stream stream, T listener ) {
        this.stream = stream;
        this.inThread = new Thread( new InputLoop() );
        this.outThread = new Thread( new OutputLoop() );

        /*
         * Input readers cannot be interrupted; never think about interrupting.
         * Just use another thread to manage its life-cycle. See  https://stackoverflow.com/a/7456207
         * for further information.
         * (Wed, 11 Mar 2020 17:02:05 +0900)
         */
        this.inThread.setDaemon(true);

        // 
        this.listener = listener;
        
        //
        this.listener.notifyParent( this );
    }
    
    boolean isClosedStreamException( Throwable e ) {
        if ( e.getMessage().contains( "Stream closed") ) {
            LOGGER.log( Level.INFO,    "Stream closed:"+ e.getMessage() );
            return true;
        } else {
            return false;
        }
    }

    
    public T getListener() {
        return listener;
    }

    protected final BlockingDeque<SisoReceiverMessage> outputMessageQueue = new LinkedBlockingDeque<>();

    private static class SimpleQuitMessage implements SisoReceiverMessage {
        @Override
        public void process(SisoReceiver server, PrintStream out) {
            Thread thread = new Thread( new Runnable() {
                @Override
                public void run() {
                    System.err.println( "sleep(1000)" );
                    try {
                        Thread.sleep(1000);
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    }
                    System.err.println( "requestQuit()" );
                    server.requestQuit();
                }
            });
            thread.setDaemon( true );
            thread.start();
        }
    }
    private static class SimplePrintMessage implements SisoReceiverMessage {
        final String message;
        SimplePrintMessage( String message ) {
            super();
            this.message = message;
        }
        @Override
        public void process(SisoReceiver server, PrintStream out) {
            out.println( message );
        }
    }

    public void postMessage( SisoReceiverMessage msg ) {
        outputMessageQueue.add( msg );
    }
    public final void postMessageQuit() {
        postMessage( createQuitMessage() );
    }
    public static SisoReceiverMessage createQuitMessage() {
        return new SimpleQuitMessage();
    }
    public static SisoReceiverMessage createPrintMessage(String message) {
        return new SimplePrintMessage(message);
    }

    class EventManager {
        transient boolean startDone = false;
        transient boolean endDone = false;
        void callStart() {
            if ( ! this.startDone ) {
                this.startDone = true;
                SisoReceiver.this.listener.start( SisoReceiver.this );
            }
        }
        void callEnd() {
            if ( ! this.endDone ) {
                this.endDone = true;
                SisoReceiver.this.listener.end( SisoReceiver.this );
            }
        }
        transient boolean quitWhenInputExited = true;
        synchronized void setQuitWhenInputExited( boolean quitWhenInputExited ) {
            this.quitWhenInputExited = quitWhenInputExited;

            if ( this.exitedInput && this.quitWhenInputExited ) {
                callEnd();
            }
        }
        transient boolean exitedInput = false;
        synchronized void notifyStartInput() {
            callStart();
        }
        synchronized void notifyEndInput() {
            this.exitedInput = true;
            if ( this.quitWhenInputExited ) {
                callEnd();
            }
        }
    }
    final EventManager eventManager = new EventManager();
    
    private class InputLoop implements Runnable {
        @Override
        public void run() {
            logInfo( "Now start the input-loop." );
            try {
                eventManager.notifyStartInput();
                boolean first = true;
                for (;;) {
                    String s = i.readLine();

                    //
                    if ( first ) {
                        first = false;
                        if ( s!= null && s.startsWith( SHEBANG )) {
                            continue;
                        }
                    }
                    
                    try {
                        synchronized ( listener ) {
                            listener.process( SisoReceiver.this, s);
                        }
                    } catch ( Throwable t ) {
                        logError("",t);
                    }
                    if ( s == null) {
                        break;
                    }
                }
            } catch (IOException e) {
                if ( ! isClosedStreamException(e) )
                    logError("", e);
            }
            eventManager.notifyEndInput();
            
            logInfo( "Exited the input-loop." );
            //			requestQuit();
        }
    }

    public void process( Collection<String> lines ) {
        synchronized ( listener ) {
            for ( String s : lines ) {
                logInfo( "process-line:> " + s );
                listener.process( SisoReceiver.this, s );
            }
        }
    }

    private class OutputLoop implements Runnable {
        @Override
        public void run() {
            logInfo( "Now start the output-loop." );
            try  {
                for (;;) {
                    try {
                        SisoReceiverMessage message = outputMessageQueue.take();
                        message.process( SisoReceiver.this, o );
                        o.flush();
                    } catch ( InterruptedException e ) {
                        throw e;
                    } catch ( Throwable t ) {
                        logError("",t);
                    }
                }
            } catch (InterruptedException e) {
                //				logError("", e);
            } catch (Throwable e ) {
                if ( ! isClosedStreamException(e) )
                    logError("", e);
            }
            //			requestQuit();
            logInfo( "Exited the output-loop." );
        }
    }

    private ApplicationComponent parentApplicationComponent;
    @Override
    public ApplicationComponent getParentApplicationComponent() {
        return this.parentApplicationComponent;
    }
    @Override
    public void setParentApplicationComponent(ApplicationComponent parentApplicationComponent) {
        this.parentApplicationComponent = parentApplicationComponent;
    }

    @Override
    public void processInit() {
        this.in  = this.stream.getDownwardStream();
        this.out = this.stream.getUpwardStream();
        this.o = out instanceof PrintStream ? (PrintStream)out : new PrintStream( out );
        this.i = new BufferedReader( new InputStreamReader(in));

        inThread.start();
        outThread.start();
    }

    volatile boolean done = false;
    @Override
    public synchronized void processQuit() {
        if ( done )return;
        done = true;
        Runnable r = new Runnable() {
            public void run() {
                System.err.println("quit");

                System.err.println("quit ... interrupt");
                inThread.interrupt();
                outThread.interrupt();

                System.err.println("quit ... closing resource");
                if ( stream instanceof Closeable ) {
                    try {
                        ((Closeable)stream).close();
                    } catch (IOException e) {
                        logWarn(e);
                    }
                }

                System.err.println("quit ... closing output");
                o.flush();
                o.close();

                /*
                 * Closing input reader usually does not work. See the comment above.
                 * (Wed, 11 Mar 2020 17:02:05 +0900)
                 */
                System.err.println("quit ... closing input");
                try {
                    in.close();
                } catch (IOException e) {
                    logWarn( e );
                }
                System.err.println("closed");
                
                try {
                    getListener().end( SisoReceiver.this );
                } catch ( Throwable e ) {
                    e.printStackTrace();
                }
            }
        };
        Thread t = new Thread( r , "clean-up" );
        t.setDaemon(true);
        t.start();
    }
}

