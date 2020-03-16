package lamu;

import lamu.lib.scheme.SchemeEvaluatorUtils;

public class notes implements Runnable {
    @Override
    public void run() {
        SchemeEvaluatorUtils.executeInTheCurrentContext( this.getClass(), "lib/basic-notes.scm"  );
    }
}
