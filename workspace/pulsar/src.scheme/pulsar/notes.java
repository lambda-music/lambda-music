package pulsar;

import quartz.lib.scheme.SchemeEvaluatorUtils;

public class notes implements Runnable {
    @Override
    public void run() {
        SchemeEvaluatorUtils.executeInTheCurrentContext( Pulsar.class, "lib/basic-notes.scm"  );
    }
}
