package pulsar;

import pulsar.lib.scheme.SchemeEvaluatorUtils;

public class music implements Runnable {
    @Override
    public void run() {
        SchemeEvaluatorUtils.executeInTheCurrentContext( Pulsar.class, "lib/music.scm" );
    }
}
