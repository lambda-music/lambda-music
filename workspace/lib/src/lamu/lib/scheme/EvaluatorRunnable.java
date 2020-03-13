package lamu.lib.scheme;

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
        Evaluator.logInfo( schemeScript );
        receiver.receive( schemeScript, 
            this.evaluator.evaluate( 
                threadInitializer, 
                schemeScript, 
                currentDirectory, 
                currentFile, 
                currentURI ) );
    }
}