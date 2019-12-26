package kawapad;

public final class KawapadEvaluatorRunnable implements Runnable {
    Kawapad kawapad;
    String schemeScript;
    KawapadEvaluator         evaluator;
    KawapadEvaluatorReceiver receiver;
    public KawapadEvaluatorRunnable( Kawapad kawapad, String schemeScript, KawapadEvaluator evaluator, KawapadEvaluatorReceiver receiver ) {
        super();
        this.kawapad      = kawapad;
        this.schemeScript = schemeScript;
        this.evaluator    = evaluator;
        this.receiver = receiver;
    }
    @Override
    public void run() {
        Kawapad.logInfo( schemeScript );
        receiver.receive( schemeScript, this.evaluator.evaluate( kawapad, schemeScript ) );
    }
}