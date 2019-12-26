package kawapad;

final class KawapadEvaluator3Remote implements KawapadEvaluator3, KawapadName {
    String url;
    public KawapadEvaluator3Remote(String url) {
        super();
        this.url = url;
    }
    
    @Override
    public String getName() {
        return this.url;
    }

    @Override
    public void evaluate( Kawapad kawapad, String text, boolean doInsertText, boolean doReplaceText, boolean doReset ) {
        kawapad.getThreadManager().startScratchPadThread( 
            new KawapadEvaluatorRunnable( 
                kawapad, text, new KawapadEvaluator1Remote(url),
                doInsertText, doReplaceText, true, doReset ) );
    }
}