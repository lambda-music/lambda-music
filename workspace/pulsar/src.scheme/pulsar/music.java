package pulsar;

import quartz.lib.scheme.SchemeEvaluatorUtils;

public class music implements Runnable {
    @Override
    public void run() {
        SchemeEvaluatorUtils.executeInTheCurrentContext( Pulsar.class, "lib/music.scm" );
    }
}
