package lamu;

import gnu.mapping.Environment;
import lamu.lib.ForceLoadingClass;
import lamu.lib.evaluators.SchemeEvaluatorLib;

public class scheme implements Runnable {
    @Override
    public void run() {
        SchemeEvaluatorLib.initScheme( Environment.getCurrent() );
    }
    static {
        ForceLoadingClass.force(SchemeEvaluatorLib.class);
    }
}
