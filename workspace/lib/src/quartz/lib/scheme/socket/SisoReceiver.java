package quartz.lib.scheme.socket;

import java.io.BufferedReader;
import java.io.Closeable;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintStream;
import java.lang.invoke.MethodHandles;
import java.util.concurrent.BlockingDeque;
import java.util.concurrent.LinkedBlockingDeque;
import java.util.logging.Level;
import java.util.logging.Logger;

import quartz.lib.CurrentObject;
import quartz.lib.app.ApplicationComponent;
import quartz.lib.log.SimpleConsoleLogger;
import quartz.lib.thread.ThreadInitializer;
import quartz.lib.thread.ThreadInitializerCollection;
import quartz.lib.thread.ThreadInitializerCollectionContainer;
import quartz.lib.thread.ThreadInitializerContainer;

/**
 * SisoServer stands for SImple SOcket Server.
 * 
 */
public class SisoReceiver implements ThreadInitializerContainer<SisoReceiver>, ThreadInitializerCollectionContainer, ApplicationComponent {
	protected static final Logger LOGGER = SimpleConsoleLogger.getLogger(MethodHandles.lookup().lookupClass().getName());
	protected static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
	protected static void logInfo(String msg) { LOGGER.log(Level.INFO, msg); }
	protected static void logWarn(String msg) { LOGGER.log(Level.WARNING, msg); }
	protected static void logWarn(Throwable e) { LOGGER.log(Level.WARNING, "warning", e); }

	protected static interface SisoMessage {
		void process( SisoReceiver server, PrintStream out ) throws IOException;
	}
	protected static interface SisoListener {
		void process( SisoReceiver server, String s );
	}

	protected final Closeable resource;
	protected final Thread inThread;
	protected final Thread outThread;
	protected final InputStream in;
	protected final OutputStream out;
	protected BufferedReader i;
	protected PrintStream o;
	protected SisoListener listener;
	public SisoReceiver( InputStream in, OutputStream out, SisoListener listener ) {
		this( null, in, out, listener );
	}
	public SisoReceiver( Closeable resource, InputStream in, OutputStream out, SisoListener listener ) {
		this.resource = resource;
		this.in = in;
		this.out = out;
		this.inThread = new Thread( new InputLoop() );
		this.outThread = new Thread( new OutputLoop() );
		
		/*
		 * Input readers cannot be interrupted; never think about interrupting.
		 * Just use another thread to manage its life-cycle. See  https://stackoverflow.com/a/7456207
		 * for further information.
		 * (Wed, 11 Mar 2020 17:02:05 +0900)
		 */
		this.inThread.setDaemon(true);
		
		this.o = out instanceof PrintStream ? (PrintStream)out : new PrintStream( out );
		this.i = new BufferedReader( new InputStreamReader(in));
		this.listener = listener;
	}
	
	protected final BlockingDeque<SisoMessage> outputMessageQueue = new LinkedBlockingDeque<>();
	
	private static class SimpleQuitMessage implements SisoMessage {
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
	private static class SimplePrintMessage implements SisoMessage {
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
	
	public void postMessage( SisoMessage msg ) {
		outputMessageQueue.add( msg );
	}
	public final void postMessageQuit() {
		postMessage( createQuitMessage() );
	}

	public static SimpleQuitMessage createQuitMessage() {
		return new SimpleQuitMessage();
	}
    public static SimplePrintMessage createPrintMessage(String message) {
		return new SimplePrintMessage(message);
	}

    private class InputLoop implements Runnable {
		@Override
		public void run() {
			logInfo( "Now start the input-loop." );
			try {
				for (;;) {
					String s = i.readLine();
					listener.process( SisoReceiver.this, s);
					if ( s == null) {
						break;
					}
				}
			} catch (IOException e) {
				logError("", e);
				
			}
			logInfo( "Exited the input-loop." );
//			requestQuit();
		}
	}

	private class OutputLoop implements Runnable {
		@Override
		public void run() {
			logInfo( "Now start the output-loop." );
			try  {
				for (;;) {
					SisoMessage message = outputMessageQueue.take();
					message.process( SisoReceiver.this, o );
				}
			} catch (IOException e) {
				logError("", e);
			} catch (InterruptedException e) {
//				logError("", e);
			} catch (Throwable e ) {
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
    			if ( resource != null ) {
    				try {
    					resource.close();
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
    				// TODO Auto-generated catch block
    				e.printStackTrace();
    			}
    			System.err.println("closed");
    		}
    	};
    	Thread t = new Thread( r , "clean-up" );
    	t.setDaemon(true);
    	t.start();
    }

    ////////////////////////////////////////////////////////////////////////////
    // The Thread Initializer Facility
    ////////////////////////////////////////////////////////////////////////////

    private static final CurrentObject<SisoReceiver> currentObject = new CurrentObject<>( SisoReceiver.class );
    private final ThreadInitializer<SisoReceiver> threadInitializer = 
            ThreadInitializer.createMultipleThreadInitializer( "siso", this, 
                ThreadInitializer.createThreadInitializer( "siso-current", currentObject, this ) );
    @Override
    public ThreadInitializer<SisoReceiver> getThreadInitializer() {
        return threadInitializer;
    }    
    public static SisoReceiver getCurrent() {
        return currentObject.get();
    }
    public static boolean isPresent() {
        return currentObject.isPresent();
    }

    ////////////////////////////////////////////////////////////////////////////

    private ThreadInitializerCollection threadInitializerCollection = new ThreadInitializerCollection( "scheme", this );
    {
        threadInitializerCollection.addThreadInitializer( this.getThreadInitializer() );
    }
    public ThreadInitializerCollection getThreadInitializerCollection() {
        return threadInitializerCollection;
    }
}

