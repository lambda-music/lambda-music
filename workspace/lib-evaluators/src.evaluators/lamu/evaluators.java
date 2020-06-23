package lamu;

import gnu.mapping.Environment;
import lamu.lib.ForceLoadingClass;
import lamu.lib.evaluators.EvaluatorLib;
import lamu.lib.kawautils.SchemeEvaluation;

public class evaluators implements Runnable {
    @Override
    public void run() {
        EvaluatorLib.initScheme( Environment.getCurrent() );
        SchemeEvaluation.executeResourceInTheCurrentContext( 
            this.getClass().getResource( "lib/evaluators.scm" ) );
    }
    static {
        ForceLoadingClass.force(EvaluatorLib.class);
    }

}
