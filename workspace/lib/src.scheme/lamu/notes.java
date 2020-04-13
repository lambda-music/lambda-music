package lamu;

import lamu.lib.scheme.SchemeEvaluatorUtils;

public class notes implements Runnable {
    @Override
    public void run() {
        SchemeEvaluatorUtils.executeResourceInTheCurrentContext( this.getClass(), "lib/basic-notes.scm"  );
    }
}
