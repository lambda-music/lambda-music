package kawapad;

import pulsar.lib.scheme.SchemeResult;

public interface KawapadEvaluatorReceiver {
    void receive( String schemeScript, SchemeResult schemeResult );
}
