package lamu;

import lamu.lib.evaluators.SchemeEvaluatorUtils;

public class notes implements Runnable {
    @Override
    public void run() {
        SchemeEvaluatorUtils.executeResourceInTheCurrentContext( this.getClass(), "lib/basic-notes.scm"  );
    }
}
