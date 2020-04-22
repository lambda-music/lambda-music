package lamu;

import lamu.lib.kawautils.SchemeEvaluation;

public class notes implements Runnable {
    @Override
    public void run() {
        SchemeEvaluation.executeResourceInTheCurrentContext( this.getClass(), "lib/basic-notes.scm"  );
    }
}
