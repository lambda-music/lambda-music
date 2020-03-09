package lamu;

import quartz.lib.scheme.SchemeEvaluatorUtils;

public class notes implements Runnable {
    @Override
    public void run() {
        SchemeEvaluatorUtils.executeInTheCurrentContext( this.getClass(), "lib/basic-notes.scm"  );
    }
}
