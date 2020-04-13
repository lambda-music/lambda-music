package lamu;

import lamu.lib.scheme.SchemeEvaluatorUtils;

public class music implements Runnable {
    @Override
    public void run() {
        SchemeEvaluatorUtils.executeResourceInTheCurrentContext( this.getClass(), "lib/music.scm" );
    }
}
