package lamu;

import quartz.lib.scheme.SchemeEvaluatorUtils;

public class music implements Runnable {
    @Override
    public void run() {
        SchemeEvaluatorUtils.executeInTheCurrentContext( this.getClass(), "lib/music.scm" );
    }
}
