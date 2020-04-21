package lamu;

import lamu.lib.evaluators.SchemeEvaluatorUtils;

public class xnoop implements Runnable {
    @Override
    public void run() {
        SchemeEvaluatorUtils.executeResourceInTheCurrentContext( this.getClass(), "lib/xnoop.scm"  );
    }
}
