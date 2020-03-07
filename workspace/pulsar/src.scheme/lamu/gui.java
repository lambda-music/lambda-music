package lamu;

import gnu.mapping.Environment;
import pulsar.lib.swing.PulsarGuiUtils;

public class gui implements Runnable {
    @Override
    public void run() {
        PulsarGuiUtils.initScheme( Environment.getCurrent() );
    }
}
