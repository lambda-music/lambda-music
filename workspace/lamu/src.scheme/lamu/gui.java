package lamu;

import gnu.expr.Language;
import kawa.standard.Scheme;
import lamu.lib.swing.PulsarGuiUtils;

public class gui implements Runnable {
    @Override
    public void run() {
        PulsarGuiUtils.initScheme( (Scheme) Language.getDefaultLanguage() );
    }
}
