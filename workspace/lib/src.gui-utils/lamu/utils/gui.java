package lamu.utils;

import gnu.mapping.Environment;
import lamu.utils.lib.PulsarGuiUtils;

public class gui implements Runnable {
    @Override
    public void run() {
        PulsarGuiUtils.initScheme( Environment.getCurrent() );
    }
}
