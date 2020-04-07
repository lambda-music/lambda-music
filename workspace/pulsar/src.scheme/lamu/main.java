package lamu;

import gnu.mapping.Environment;
import pulsar.PulsarLib;

public class main implements Runnable {
    @Override
    public void run() {
        PulsarLib.initScheme( Environment.getCurrent() );
    }
}
