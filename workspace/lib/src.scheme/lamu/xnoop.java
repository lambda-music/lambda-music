package lamu;

import lamu.lib.scheme.SchemeEvaluatorUtils;

public class xnoop implements Runnable {
    @Override
    public void run() {
        SchemeEvaluatorUtils.executeResourceInTheCurrentContext( this.getClass(), "lib/xnoop.scm"  );
    }
}
