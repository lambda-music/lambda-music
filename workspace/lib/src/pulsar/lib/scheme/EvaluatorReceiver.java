package pulsar.lib.scheme;

public interface EvaluatorReceiver {
    EvaluatorReceiver REPORT_ERROR = new EvaluatorReceiver() {
        @Override
        public void receive(String schemeScript, SchemeResult schemeResult) {
            if ( ! schemeResult.isSucceeded() ) {
                Evaluator.logError( "failed", schemeResult.getError() );
            }
        }
    };
    void receive( String schemeScript, SchemeResult schemeResult );
}
