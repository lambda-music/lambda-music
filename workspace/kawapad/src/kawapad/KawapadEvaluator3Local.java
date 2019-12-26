package kawapad;

final class KawapadEvaluator3Local implements KawapadEvaluator3, KawapadName {
    @Override
    public void evaluate( Kawapad kawapad, String text, boolean doInsertText, boolean doReplaceText, boolean doReset ) {
        kawapad.getThreadManager().startScratchPadThread( 
            new KawapadEvaluatorRunnable( 
                kawapad, text,
                new KawapadEvaluator1Local(),
                doInsertText, doReplaceText, true, doReset ) );
    }
    @Override
    public String getName() {
        return "local";
    }
}