package kawapad;

import pulsar.lib.scheme.SchemeExecutor;
import pulsar.lib.scheme.SchemeResult;

final class KawapadEvaluatorLocal implements KawapadEvaluator {
    @Override
    public SchemeResult evaluate(Kawapad kawapad, String schemeScript) {
        return SchemeExecutor.evaluateScheme( 
            kawapad.getSchemeEngine().getSchemeExecutor(), 
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