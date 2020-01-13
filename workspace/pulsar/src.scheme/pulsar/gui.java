package pulsar;

import gnu.expr.Language;
import kawa.standard.Scheme;

public class gui implements Runnable {
    @Override
    public void run() {
        PulsarFramePackage.initScheme( (Scheme) Language.getDefaultLanguage() );
    }
}
