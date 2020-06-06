package lamu;

import lamu.lib.kawautils.SchemeEvaluation;

public class song implements Runnable {
    @Override
    public void run() {
        SchemeEvaluation.executeResourceInTheCurrentContext( this.getClass(), "lib/song.scm" );
    }
}
