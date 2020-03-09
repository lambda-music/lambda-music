package lamu;

import quartz.lib.scheme.SchemeEvaluatorUtils;

public class xnoop implements Runnable {
    @Override
    public void run() {
        SchemeEvaluatorUtils.executeInTheCurrentContext( this.getClass(), "lib/xnoop.scm"  );
    }
}
