package lamu.lib.evaluators;

import java.io.File;

class EvaluatorRunnable implements Runnable {
    final Runnable threadInitializer;
    final String schemeScript;
    final Evaluator evaluator;
    final EvaluatorReceiver receiver;
    final File currentDirectory;
    final File currentFile;
    final String currentURI;
    EvaluatorRunnable(
        Runnable threadInitializer,
        String schemeScript,
        Evaluator evaluator, 
        EvaluatorReceiver receiver, 
        File currentDirectory, 
        File currentFile,
        String currentURI )
    {
        super();
        this.threadInitializer = threadInitializer;
        this.schemeScript = schemeScript;
        this.evaluator    = evaluator;
        this.receiver     = receiver;
        this.currentDirectory = currentDirectory;
        this.currentFile = currentFile;
        this.currentURI = currentURI;
    }
    @Override
    public void run() {
        try {
            Evaluator.logInfo( schemeScript );
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