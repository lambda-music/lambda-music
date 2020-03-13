package lamu.lib.scheme;

import java.lang.invoke.MethodHandles;
import java.util.ArrayDeque;
import java.util.logging.Level;
import java.util.logging.Logger;

import lamu.lib.log.LamuLogger;

public final class ThreadManager {
    static final Logger LOGGER = LamuLogger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }

    private static final boolean DEBUG = false;
    private final class ScratchPadThread extends Thread {
        private final Runnable r;
        private ScratchPadThread(Runnable r) {
            this.r = r;
        }

        @Override
        public void run() {
            try {
                if ( DEBUG )
                    logInfo( "ScratchPadThreadManager:run" );
                // ==== WORKAROUND SEE acvpoeov === (Tue, 23 Jul 2019 11:37:32 +0900) //
//                      schemeSecretary.initializeSchemeForCurrentThread();
                // ==== WORKAROUND SEE acvpoeov === (Tue, 23 Jul 2019 11:37:32 +0900) //
                r.run();
            } catch ( Throwable t ) {
                logError( "error occured in " + r , t );
            } finally {
                if ( DEBUG )
                    logInfo( "ScratchPadThreadManager:end" );
                removeThread( this );
            }
        }
        @Override
        public void interrupt() {
            logInfo("interrupted");
            super.interrupt();
        }
    }
    private ArrayDeque<Thread> threadList = new ArrayDeque<>();
    private void addThread( Thread t ) {
        synchronized ( threadList ) {
            threadList.add( t );
        }
    }
    private void removeThread( Thread t ) {
        synchronized ( threadList ) {
            threadList.remove( t );
        }
    }
    public void startThread( Runnable r ) {
        Thread t = new ScratchPadThread(r);
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