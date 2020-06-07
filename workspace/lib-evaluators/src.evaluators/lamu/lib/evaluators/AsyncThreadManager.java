package lamu.lib.evaluators;

import java.lang.invoke.MethodHandles;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.logging.Level;

import lamu.lib.log.Logger;

public final class AsyncThreadManager implements ThreadManager {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }

    private static final boolean DEBUG = false;
    public static final class AsyncThreadManagerThread extends Thread {
        private final AsyncThreadManager threadManager;
        private final Runnable r;
        private volatile Date startDate=null;
        private volatile Date endDate=null;
        private AsyncThreadManagerThread(AsyncThreadManager threadManager, Runnable r) {
            this.threadManager = threadManager;
            this.r = r;
        }

        @Override
        public void run() {
            // Invoke the thread initializer.
            this.threadManager.setCurrentThreadManager();
            try {
                this.startDate = new Date(); 
                if ( DEBUG )
                    logInfo( "ScratchPadThreadManager:run" );
                // ==== WORKAROUND SEE acvpoeov === (Tue, 23 Jul 2019 11:37:32 +0900) //
//                      schemeSecretary.initializeSchemeForCurrentThread();
                // ==== WORKAROUND SEE acvpoeov === (Tue, 23 Jul 2019 11:37:32 +0900) //
                r.run();
            } catch ( Throwable t ) {
                logError( "error occured in " + r , t );
            } finally {
                this.endDate = new Date(); 
                if ( DEBUG )
                    logInfo( "ScratchPadThreadManager:end" );
                this.threadManager.removeThread( this );
            }
        }
        @Override
        public void interrupt() {
            logInfo("interrupted");
            super.interrupt();
        }
        public Date getStartDate() {
            return startDate;
        }
        public Date getEndDate() {
            return endDate;
        }
        @Override
        public String toString() {
            return 
                String.format(
                    "(#thread %s %s)",
                    ( startDate == null ? "not-started"  : startDate.toString()),
                    ( endDate == null   ? "not-finished" : endDate.toString())
                    );
        }
    }
    private ArrayDeque<AsyncThreadManagerThread> threadList = new ArrayDeque<>();
    void addThread( AsyncThreadManagerThread t ) {
        synchronized ( threadList ) {
            threadList.add( t );
        }
    }
    void removeThread( AsyncThreadManagerThread t ) {
        synchronized ( threadList ) {
            threadList.remove( t );
        }
    }
    public List<AsyncThreadManagerThread> getThreads() {
        synchronized ( threadList ) {
            return new ArrayList<AsyncThreadManagerThread>( threadList );
        }
    }
    public void startThread( Runnable r ) {
        AsyncThreadManagerThread t = new AsyncThreadManagerThread(this,r);
        addThread(t);
        t.start();
    }
    public void interruptAllThreads() {
        logInfo("interruptScratchPadThreads");
        synchronized ( threadList ) {
            for ( Thread t : threadList ) {
                logInfo( "interrupt start" );
                t.interrupt();
                logInfo( "interrpt end" );
            }
        }
    }
}