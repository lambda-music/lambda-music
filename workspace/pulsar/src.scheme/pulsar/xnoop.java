package pulsar;

import pulsar.lib.scheme.SchemeEvaluatorUtils;

public class xnoop implements Runnable {
    @Override
    public void run() {
        SchemeEvaluatorUtils.executeInTheCurrentContext( Pulsar.class, "lib/xnoop.scm"  );
    }
}
