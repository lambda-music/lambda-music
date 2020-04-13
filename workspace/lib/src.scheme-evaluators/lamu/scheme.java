package lamu;

import gnu.mapping.Environment;
import lamu.lib.scheme.SchemeEngineLib;
import lamu.lib.scheme.SchemeEvaluatorLib;

public class scheme implements Runnable {
    @Override
    public void run() {
        SchemeEngineLib.initScheme( Environment.getCurrent() );
        SchemeEvaluatorLib.initScheme( Environment.getCurrent() );
    }
}
