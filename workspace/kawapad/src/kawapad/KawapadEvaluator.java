package kawapad;

public interface KawapadEvaluator {
    void evaluate( Kawapad kawapad, String text,  boolean doInsertText, boolean doReplaceText, boolean doReset);
    public static KawapadEvaluator getLocal() {
        return new KawapadLocalEvaluator();
    }

    public static KawapadEvaluator getRemote( String url ) {
        return new KawapadRemoteEvaluator( url );
    }
}
