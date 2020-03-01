package lamu;

import lamu.Pulsar;
import lamu.lib.scheme.SchemeEvaluatorUtils;

public class music implements Runnable {
    @Override
    public void run() {
        SchemeEvaluatorUtils.executeInTheCurrentContext( Pulsar.class, "lib/music.scm" );
    }
}
