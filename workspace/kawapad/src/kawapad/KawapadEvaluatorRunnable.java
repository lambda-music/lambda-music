package kawapad;

import pulsar.lib.scheme.Evaluator;
import pulsar.lib.scheme.EvaluatorReceiver;
import pulsar.lib.scheme.SchemeEngine;

public class KawapadEvaluatorRunnable  {
    public static Runnable create(
            Kawapad kawapad, 
            String schemeScript, 
            Evaluator evaluator,
            EvaluatorReceiver receiver) {
        return SchemeEngine.create( 
            kawapad.getThreadInitializerCollection(), 
            schemeScript, 
            evaluator, 
            receiver, 
            kawapad.getCurrentDirectory(), 
            kawapad.getCurrentFile(), 
            "scratchpad"
            );
    }
}