package lamu;

import lamu.lib.kawautils.SchemeEvaluation;

public class lang implements Runnable {
    @Override
    public void run() {
        SchemeEvaluation.executeResourceInTheCurrentContext( this.getClass(), "./lang.scm" );
    }
}
