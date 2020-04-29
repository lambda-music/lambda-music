package lamu;

import gnu.mapping.Environment;
import lamu.lib.ForceLoadingClass;
import lamu.lib.evaluators.EvaluatorLib;

public class evaluators implements Runnable {
    @Override
    public void run() {
        EvaluatorLib.initScheme( Environment.getCurrent() );
    }
    static {
        ForceLoadingClass.force(EvaluatorLib.class);
    }

}
