package lamu;

import gnu.expr.Language;
import kawa.standard.Scheme;
import lamu.Pulsar;

public class main implements Runnable {
    @Override
    public void run() {
        Pulsar.initScheme( (Scheme) Language.getDefaultLanguage() );
    }
}
