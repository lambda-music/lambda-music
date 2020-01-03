package kawapad;

import pulsar.lib.scheme.SchemeResult;

final class KawapadEvaluatorLocal implements KawapadEvaluator {
    @Override
    public SchemeResult evaluate(Kawapad kawapad, String schemeScript) {
        return kawapad.getSchemeEngine().getSchemeExecutor().evaluate( 
        kawapad.getThreadInitializerCollection(),    
        schemeScript, 
        kawapad.getCurrentDirectory(), 
        kawapad.getCurrentFile(), 
        "scratchpad" );
    }
    @Override
    public String getName() {
        return "local";
    }
}