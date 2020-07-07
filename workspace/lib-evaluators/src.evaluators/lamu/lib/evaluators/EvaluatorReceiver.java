package lamu.lib.evaluators;

import lamu.lib.logging.SimpleConsole;

public interface EvaluatorReceiver {
    EvaluatorReceiver REPORT_ERROR = new EvaluatorReceiver() {
        @Override
        public void receive(SchemeResult schemeResult) {
//        	  MODIFIED (Fri, 13 Mar 2020 17:29:37 +0900) >>>
//            if ( ! schemeResult.isSucceeded() ) {
//                Evaluator.logError( "failed", schemeResult.getError() );
//            }
//      	  MODIFIED (Fri, 13 Mar 2020 17:29:37 +0900) <<<
        	schemeResult.warnIfError();
        	SimpleConsole.getConsole().addWarning( schemeResult.getError());
        }
    };
    void receive( SchemeResult schemeResult );
}
