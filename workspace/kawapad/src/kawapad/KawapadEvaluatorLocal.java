package kawapad;

import pulsar.lib.scheme.SchemeResult;
import pulsar.lib.scheme.scretary.SchemeExecutor;

final class KawapadEvaluatorLocal implements KawapadEvaluator {
    @Override
    public SchemeResult evaluate(Kawapad kawapad, String schemeScript) {
        return SchemeExecutor.evaluateScheme( 
            kawapad.getSchemeExecutor(), 
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