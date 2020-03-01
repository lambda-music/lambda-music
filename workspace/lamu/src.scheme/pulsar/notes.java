package pulsar;

import pulsar.lib.scheme.SchemeEvaluatorUtils;

public class notes implements Runnable {
    @Override
    public void run() {
        SchemeEvaluatorUtils.executeInTheCurrentContext( Pulsar.class, "lib/basic-notes.scm"  );
    }
}
