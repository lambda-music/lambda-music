package lamu.lib.scheme;

import java.io.File;

public class AsyncEvaluator {
    public static void executeAsync(
        ThreadManager threadManager,
        Runnable threadInitializer, 
        String schemeScript, 
        Evaluator evaluator, 
        EvaluatorReceiver receiver,
        File currentDirectory, 
        File currentFile, 
        String currentURI)
    {
        threadManager.startThread(
            createEvaluationRunner(
                threadInitializer, 
                schemeScript, 
                evaluator, 
                receiver, 
                currentDirectory, 
                currentFile, 
                currentURI ));
    }
    public static Runnable createEvaluationRunner(
        Runnable threadInitializer, 
        String schemeScript,
        Evaluator evaluator,
        EvaluatorReceiver receiver, 
        File currentDirectory, 
        File currentFile, 
        String currentURI )
    {
        return new EvaluatorRunnable( 
            threadInitializer, 
            schemeScript, 
            evaluator, 
            receiver, 
            currentDirectory,
            currentFile, 
            currentURI );
    }
}
