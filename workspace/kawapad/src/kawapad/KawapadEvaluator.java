package kawapad;

import java.io.File;

import pulsar.lib.scheme.SchemeEvaluator;
import pulsar.lib.scheme.SchemeResult;

public interface KawapadEvaluator extends KawapadName {
    SchemeResult evaluate( SchemeEvaluator evaluator, Runnable threadInitializer, String schemeScript, File currentDirectory, File currentFile, String currentURI );
    public static KawapadEvaluator getLocal() {
        return new KawapadEvaluatorLocal();
    }
    public static KawapadEvaluator getRemote( String url ) {
        return new KawapadEvaluatorRemote( url );
    }
}
