package pulsar;

import gnu.expr.Language;
import kawa.standard.Scheme;

public class main implements Runnable {
    @Override
    public void run() {
        Pulsar.initScheme( (Scheme) Language.getDefaultLanguage() );
    }
}
