package lamu;

import gnu.mapping.Environment;
import kawapad.Kawapad;
import kawapad.KawapadFrame;
import lamu.lib.kawautils.SchemeValues;
import lamu.lib.kawautils.procedures.MultipleNamedProcedure0;

public class kawapad implements Runnable {
    public static final String DOCS_ID = "kawapad-gui-procedures";
        
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

    @Override
    public void run() {
        Environment env = Environment.getCurrent() ;
        SchemeValues.defineLambda( env, currentKawapadProc );
        SchemeValues.defineLambda( env, currentKawapadFrameProc );

    }
}
