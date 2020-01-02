package kawapad;

import pulsar.lib.scheme.SchemeEngine;
import pulsar.lib.scheme.SchemeResult;

final class KawapadEvaluatorLocal implements KawapadEvaluator {
    @Override
    public SchemeResult evaluate(Kawapad kawapad, String schemeScript) {
        return SchemeEngine.evaluateScheme( 
            kawapad.getSchemeEngine(), 
            kawapad.getThreadInitializerCollection(),
            schemeScript, 
            kawapad.getCurrentDirectory(), 
            kawapad.getCurrentFile(), "scratchpad" );
    }
    @Override
    public String getName() {
        return "local";
    }
}