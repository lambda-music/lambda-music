package lamu;

import gnu.mapping.Environment;
import pulsar.Pulsar;

public class main implements Runnable {
    @Override
    public void run() {
        Pulsar.initScheme( Environment.getCurrent() );
    }
}
