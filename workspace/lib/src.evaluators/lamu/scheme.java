package lamu;

import gnu.mapping.Environment;
import lamu.lib.evaluators.SchemeEngineLib;
import lamu.lib.evaluators.SchemeEvaluatorLib;

public class scheme implements Runnable {
    @Override
    public void run() {
        SchemeEngineLib.initScheme( Environment.getCurrent() );
        SchemeEvaluatorLib.initScheme( Environment.getCurrent() );
    }
}
