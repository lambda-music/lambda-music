package metro;

import java.lang.invoke.MethodHandles;
import java.util.logging.Level;

import lamu.lib.Invokable;
import lamu.lib.logging.Logger;

public class SetTracing implements Invokable {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }
    public static final SetTracing TRON  = new SetTracing(true);
    public static final SetTracing TROFF = new SetTracing(false);
    private final boolean enabled;
    private SetTracing( boolean enabled ) {
		this.enabled = enabled;
	}
	@Override
	public Object invoke(Object... args) {
		for ( Object o : args ) {
			if ( o instanceof MetroTrack ) {
				MetroTrack track = (MetroTrack) o;
				MetroSequence sequence = track.getSequence();
				if ( sequence instanceof MetroTraceableSequence ) {
					((MetroTraceableSequence)sequence).setTracingEnabled(enabled);
				} else {
					logWarn( o + " has no MetroTraceableSequence; ignored." );
				}
			} else {
				logWarn( o + " is not a MetroTrack; ignored." );
			}
		}
		return null;
	}
}
