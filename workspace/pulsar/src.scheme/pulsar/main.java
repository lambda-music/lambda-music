package pulsar;

import gnu.mapping.Environment;

public class main implements Runnable {
    @Override
    public void run() {
        Pulsar.initScheme( Environment.getCurrent() );
    }
}
