package kawapad;

import java.lang.invoke.MethodHandles;
import java.util.logging.Level;
import java.util.logging.Logger;

import pulsar.lib.scheme.SchemeResult;

public interface KawapadEvaluatorReceiver {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }

    KawapadEvaluatorReceiver REPORT_ERROR = new KawapadEvaluatorReceiver() {
        @Override
        public void receive(String schemeScript, SchemeResult schemeResult) {
            if ( ! schemeResult.isSucceeded() ) {
                logError( "failed", schemeResult.getError() );
            }
        }
    };
    void receive( String schemeScript, SchemeResult schemeResult );
}
