package kawapad;

public interface KawapadEvaluator3 {
    void evaluate( Kawapad kawapad, String text,  boolean doInsertText, boolean doReplaceText, boolean doReset );
    public static KawapadEvaluator3 getLocal() {
        return new KawapadEvaluator3Local();
    }
    public static KawapadEvaluator3 getRemote( String url ) {
        return new KawapadEvaluator3Remote( url );
    }
}
