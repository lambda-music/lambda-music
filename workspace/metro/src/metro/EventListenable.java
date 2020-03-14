package metro;


import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Level;

import lamu.lib.log.Logger;

public interface EventListenable {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }

    public void clearEventListeners();
    public void addEventListener( Object type, Listener listener );
    public void removeEventListener( Listener listener );
    public void invokeEventListener( Object type );
    
    public interface Listener {
        void occured( Object parent, Object type );
    }

    public static class Default implements EventListenable {
        private static class Entry {
            Object type;
            Listener listener;
            public Entry(Object type, Listener listener) {
                super();
                this.type = type;
                this.listener = listener;
            }
        }
        private final Object parent;
        private final List<Entry> entries = new ArrayList<>();
        public Default(Object parent) {
            super();
            this.parent = parent;
        }
        @Override
        public void clearEventListeners() {
            entries.clear();
        }
        @Override
        public void addEventListener(Object type, Listener listener) {
            entries.add( new Entry( type, listener ) );
        }
        @Override
        public void invokeEventListener(Object type) {
            // duplicate the list since event-listeners possiblly modify the element list.
            // (Tue, 24 Sep 2019 10:47:10 +0900)
            List<Entry> duplicatedList = new ArrayList<>( entries );
            for ( Iterator<Entry> i=duplicatedList.iterator(); i.hasNext(); ) {
                Entry e = i.next();
                if ( e.type.equals( type ) ) {
                    try {
                        e.listener.occured( parent, type );
                    } catch ( Throwable t ) {
                        logError( "Ignored", t );
                    }
                }
            }
        }
        @Override
        public void removeEventListener(Listener listener) {
            for ( Iterator<Entry> i=entries.iterator(); i.hasNext(); ) {
                Entry e = i.next();
                if ( e.listener.equals( listener ) ) {
                    i.remove();
                }
            }
        }
    }
}
