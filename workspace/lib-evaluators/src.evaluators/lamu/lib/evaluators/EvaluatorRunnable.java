package lamu.lib.evaluators;

import java.io.File;
import java.io.Reader;

class EvaluatorRunnable implements Runnable {
    final Runnable threadInitializer;
    final Reader schemeScript;
    final Evaluator evaluator;
    final EvaluatorReceiver receiver;
    final File currentFile;
    final String currentURI;
    EvaluatorRunnable(
        Runnable threadInitializer,
        Reader schemeScript,
        Evaluator evaluator, 
        EvaluatorReceiver receiver, 
        File currentFile, 
        String currentURI )
    {
        super();
        this.threadInitializer = threadInitializer;
        this.schemeScript = schemeScript;
        this.evaluator    = evaluator;
        this.receiver     = receiver;
        this.currentFile = currentFile;
        this.currentURI = currentURI;
    }
    @Override
    public void run() {
        try {
            receiver.receive( this.evaluator.evaluate( 
                threadInitializer, 
                schemeScript, 
                currentFile, 
                currentURI ) );
        } catch ( Throwable t ) {
            Evaluator.logError("error:" + this.getClass(), t);
        }
    }
}