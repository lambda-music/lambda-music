package kawapad;

import java.util.ArrayDeque;

public final class KawapadThreadManager {
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
                    Kawapad.logInfo( "ScratchPadThreadManager:run" );
                // ==== WORKAROUND SEE acvpoeov === (Tue, 23 Jul 2019 11:37:32 +0900) //
//                      schemeSecretary.initializeSchemeForCurrentThread();
                // ==== WORKAROUND SEE acvpoeov === (Tue, 23 Jul 2019 11:37:32 +0900) //
                r.run();
            } finally {
                if ( DEBUG )
                    Kawapad.logInfo( "ScratchPadThreadManager:end" );
                removeScratchPadThread( this );
            }
        }
        @Override
        public void interrupt() {
            Kawapad.logInfo("interrupted");
            super.interrupt();
        }
    }
    private ArrayDeque<Thread> scratchPadThreadList = new ArrayDeque<>();
    public void addScratchPadThread( Thread t ) {
        synchronized ( scratchPadThreadList ) {
            scratchPadThreadList.add( t );
        }
    }
    public void startScratchPadThread( Runnable r ) {
        Thread t = new ScratchPadThread(r);
        addScratchPadThread(t);
        t.start();
    }
    public void removeScratchPadThread( Thread t ) {
        synchronized ( scratchPadThreadList ) {
            scratchPadThreadList.remove( t );
        }
    }
    public void interruptScratchPadThreads() {
        Kawapad.logInfo("interruptScratchPadThreads");
        synchronized ( scratchPadThreadList ) {
            for ( Thread t : scratchPadThreadList ) {
                Kawapad.logInfo( "interrupt start" );
                t.interrupt();
                Kawapad.logInfo( "interrpt end" );
            }
        }
    }
}