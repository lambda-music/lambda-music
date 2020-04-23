package lamu;

import lamu.lib.kawautils.SchemeEvaluation;

public class xnoop implements Runnable {
    @Override
    public void run() {
        SchemeEvaluation.executeResourceInTheCurrentContext( this.getClass(), "lib/xnoop.scm"  );
    }
}
