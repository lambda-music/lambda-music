package quartz.lib.secretary;

import java.lang.invoke.MethodHandles;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.logging.Level;

import quartz.lib.log.SimpleConsoleLogger;

public abstract class SecretaryMessageQueue<R> implements SecretaryMessageExecutor<R> {
    static final SimpleConsoleLogger LOGGER = SimpleConsoleLogger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }

    private boolean directMeeting = false;
    public void setDirectMeeting( boolean directMeeting ) {
        this.directMeeting = directMeeting;
    }
    public boolean isDirectMeeting() {
        return directMeeting;
    }
    
    protected abstract R getExecutive();

    static class SecretaryMessageWrapper<R,T,E extends Throwable> {
        SecretaryMessage<R,T,E> message;
        Object[] args;
        T result = null;
        Throwable throwable = null ;
        SecretaryMessageWrapper( SecretaryMessage<R,T,E> message, Object[] args  ) {
            this.message = message;
            this.args = args;
        }
        final CountDownLatch startLatch = new CountDownLatch(1);
        final CountDownLatch endLatch = new CountDownLatch(1);

        public void execute( R resource )  {
            try {
                startLatch.await();
                this.result = message.execute( resource, args );
            } catch (Throwable e) {
                this.throwable = e;
            } finally {
                endLatch.countDown();
            }
        }
    }
    public SecretaryMessageQueue() {
    }
    private final LinkedBlockingQueue<SecretaryMessageWrapper> queue = new LinkedBlockingQueue<>();

    private Runnable runnable = new Runnable() {
        @Override
        public void run() {
            try {
                while(true){
                    SecretaryMessageWrapper mw = queue.take();
                    try {
                        mw.execute( getExecutive() );
                    } catch ( Throwable t ) {
                        // this will never happen.
                        logError("", t);
                    }
                }
            } catch (InterruptedException e) {
                logInfo( "SecretaryMessageQueue.interrupted" );
            }
        }
    }; 
    private final Thread thread = new Thread( runnable, "SecretaryMessageQueue" );
    {
        thread.setDaemon( true ); 
        thread.start();
    }

    // ADDED (Tue, 06 Aug 2019 19:17:35 +0900)
    @Override
    public <T,E extends Throwable> T executeSecretarially( SecretaryMessage<R,T,E> message, Object... args ) throws E {
        return message.execute( getExecutive(), args );
    }
    
    <T,E extends Throwable> T executeSecretarially2( SecretaryMessage<R,T,E> message, Object... args ) throws E {
        if ( message == null )
            throw new NullPointerException();
        
        try {
//          System.out.println( "executeSequentially:" + Thread.currentThread().getName() );

            // Check if this call is a reentrant call. (Mon, 22 Jul 2019 14:31:58 +0900)
            // does this work? (Tue, 23 Jul 2019 04:00:55 +0900)
            if ( directMeeting || Thread.currentThread().equals( this.thread ) ) {
                return message.execute( getExecutive(), args );
            } else {
                SecretaryMessageWrapper<R,T,E> messageWrapper = new SecretaryMessageWrapper( message, args );
                this.queue.add( messageWrapper );
                messageWrapper.startLatch.countDown();
                messageWrapper.endLatch.await();
                if ( messageWrapper.throwable != null )
                    throw new RuntimeException( messageWrapper.throwable );
                else
                    return messageWrapper.result;
            }
        } catch ( InterruptedException e ) {
            throw new RuntimeException( e );
        }
    }
    
    public static void main(String[] args) throws IllegalArgumentException, InterruptedException {
        SecretaryMessageQueue<Double> kmq = new SecretaryMessageQueue<Double>() {
            @Override
            protected Double getExecutive() {
                return 3.1415;
            }
        };
        Integer i = kmq.executeSecretarially( new SecretaryMessage<Double, Integer,IllegalArgumentException>() {
            @Override
            public Integer execute( Double resouce, Object[] args ) throws IllegalArgumentException {
                try {
                    Thread.sleep(1000);
                } catch ( InterruptedException e ) {
                    e.printStackTrace();
                }
                int v = (int) (resouce + 3 + 3);
                for ( Object e : args ) {
                    v = v + ((Integer)e);
                }
                return v;
            }
        }, 2, 2 ); 
        
        System.out.println( i );
    }
}
