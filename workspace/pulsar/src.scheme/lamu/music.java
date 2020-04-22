package lamu;

import lamu.lib.kawautils.SchemeEvaluation;

public class music implements Runnable {
    @Override
    public void run() {
        SchemeEvaluation.executeResourceInTheCurrentContext( this.getClass(), "lib/music.scm" );
    }
}
