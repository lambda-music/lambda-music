package lamu;

import java.util.Collection;

import gnu.mapping.Environment;
import kawapad.Kawapad;
import kawapad.KawapadFrame;
import kawapad.KawapadHistoryPair;
import kawapad.PairFactory;
import lamu.lib.kawautils.SchemeValues;
import lamu.lib.kawautils.procedures.MultipleNamedProcedure0;
import lamu.lib.kawautils.procedures.MultipleNamedProcedureN;

public class kawapad implements Runnable {
    public static final String DOCS_ID = "kawapad-procedures";
        
    public static final CurrentKawapadProc currentKawapadProc = new CurrentKawapadProc(new String[] { "current-kawapad" });
    public static final class CurrentKawapadProc extends MultipleNamedProcedure0 {
        public CurrentKawapadProc(String[] names) {
            super(names);
        }
        @Override
        public Object apply0() throws Throwable {
            return Kawapad.getCurrent();
        }
    }
    public static final CurrentKawapadFrameProc currentKawapadFrameProc = new CurrentKawapadFrameProc(new String[] { "current-frame" });
    public static final class CurrentKawapadFrameProc extends MultipleNamedProcedure0 {
        public CurrentKawapadFrameProc(String[] names) {
            super(names);
        }
        @Override
        public Object apply0() throws Throwable {
            return KawapadFrame.getCurrent();
        }
    }

    public static final KawapadResultHistoryProc kawapadResultHistory = new KawapadResultHistoryProc(new String[] { "kawapad-history", "kph" });
    public static final class KawapadResultHistoryProc extends MultipleNamedProcedureN {
        public KawapadResultHistoryProc(String[] names) {
            super(names);
        }
        @Override
        public Object applyN( Object[] args ) throws Throwable {
            if ( args.length == 0 ) { 
                return 
                    PairFactory.makeList( KawapadHistoryPair.FACTORY, 
                        Kawapad.getCurrent().getResultHistoryAsScheme());
            } else if ( args.length == 1  ) {
                Kawapad.getCurrent().setResultHistory((Collection<Object>) args[0]);
                return args[0];
            } else {
                throw new IllegalArgumentException("the number of argument must be 0 or 1");
            }
        }
    }
    public static final ClearKawapadResultHistoryProc clearKawapadResultHistory = new ClearKawapadResultHistoryProc(new String[] { "clear-kawapad-history", "ckph" });
    public static final class ClearKawapadResultHistoryProc extends MultipleNamedProcedure0 {
        public ClearKawapadResultHistoryProc(String[] names) {
            super(names);
        }
        @Override
        public Object apply0() throws Throwable {
            Kawapad.getCurrent().clearResultHistory();
            return false;
        }
    }

    @Override
    public void run() {
        Environment env = Environment.getCurrent() ;
        SchemeValues.defineLambda( env, currentKawapadProc );
        SchemeValues.defineLambda( env, currentKawapadFrameProc );
        SchemeValues.defineLambda( env, kawapadResultHistory );
        SchemeValues.defineLambda( env, clearKawapadResultHistory );

    }
}
