package lamu.lib.evaluators;

import java.io.File;
import java.io.Reader;

public class AsyncEvaluator {
    public static void executeAsync(
        ThreadManager threadManager,
        Runnable threadInitializer, 
        Reader schemeScript, 
        Evaluator evaluator, 
        EvaluatorReceiver receiver,
        File currentFile, 
        String currentURI)
    {
        threadManager.startThread(
            createEvaluationRunner(
                threadInitializer, 
                schemeScript, 
                evaluator, 
                receiver, 
                currentFile, 
                currentURI ));
    }
    public static Runnable createEvaluationRunner(
        Runnable threadInitializer, 
        Reader schemeScript,
        Evaluator evaluator,
        EvaluatorReceiver receiver, 
        File currentFile, 
        String currentURI )
    {
        return new EvaluatorRunnable( 
            threadInitializer, 
            schemeScript, 
            evaluator, 
            receiver, 
            currentFile,
            currentURI );
    }
}
