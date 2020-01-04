package kawapad;

import java.io.File;

import pulsar.lib.scheme.SchemeEvaluator;
import pulsar.lib.scheme.SchemeResult;

final class KawapadEvaluatorLocal implements KawapadEvaluator {
    @Override
    public SchemeResult evaluate( SchemeEvaluator evaluator, Runnable threadInitializer, String schemeScript, File currentDirectory, File currentFile, String currentURI ) {
        return evaluator.evaluate( 
                    threadInitializer,    
                    schemeScript, 
                    currentDirectory, 
                    currentFile, 
                    currentURI );
    }
    @Override
    public String getName() {
        return "local";
    }
}