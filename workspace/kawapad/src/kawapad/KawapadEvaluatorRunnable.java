package kawapad;

import pulsar.lib.scheme.Evaluator;

public final class KawapadEvaluatorRunnable implements Runnable {
    Kawapad kawapad;
    String schemeScript;
    Evaluator evaluator;
    KawapadEvaluatorReceiver receiver;
    String currentURI = "scratchpad";
    public KawapadEvaluatorRunnable( Kawapad kawapad, String schemeScript, Evaluator evaluator, KawapadEvaluatorReceiver receiver ) {
        super();
        this.kawapad      = kawapad;
        this.schemeScript = schemeScript;
        this.evaluator    = evaluator;
        this.receiver     = receiver;
    }
    @Override
    public void run() {
        Kawapad.logInfo( schemeScript );
        receiver.receive( schemeScript, 
            this.evaluator.evaluate( 
                kawapad.getThreadInitializerCollection(), 
                schemeScript, 
                kawapad.getCurrentDirectory(), 
                kawapad.getCurrentFile(), 
                currentURI ) );
    }
}