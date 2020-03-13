package lamu;

import lamu.lib.scheme.SchemeEvaluatorUtils;

public class xnoop implements Runnable {
    @Override
    public void run() {
        SchemeEvaluatorUtils.executeInTheCurrentContext( this.getClass(), "lib/xnoop.scm"  );
    }
}
