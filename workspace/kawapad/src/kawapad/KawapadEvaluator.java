package kawapad;

import pulsar.lib.scheme.SchemeResult;

public interface KawapadEvaluator extends KawapadName {
    SchemeResult evaluate(Kawapad kawapad, String schemeScript);
    public static KawapadEvaluator getLocal() {
        return new KawapadEvaluatorLocal();
    }
    public static KawapadEvaluator getRemote( String url ) {
        return new KawapadEvaluatorRemote( url );
    }
}
