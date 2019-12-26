package kawapad;

import pulsar.lib.scheme.SchemeResult;

public interface KawapadEvaluator1 extends KawapadName {
    SchemeResult evaluate(Kawapad kawapad, String schemeScript);
    public static KawapadEvaluator1 getLocal() {
        return new KawapadEvaluator1Local();
    }
    public static KawapadEvaluator1 getRemote( String url ) {
        return new KawapadEvaluator1Remote( url );
    }

}
