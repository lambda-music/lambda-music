package lamu.lib.evaluators;

import java.util.ArrayList;
import java.util.List;

import gnu.lists.LList;
import gnu.mapping.Procedure;

public class EvaluatorUtils {
    public static List<String> getAllKey( Evaluator evaluator ) {
        SchemeResult result = evaluator.evaluate( 
            "(environment-fold (interaction-environment) cons '())", "get-all" );
        result.throwIfError();
        return new ArrayList<>( SchemeValues.toStringList((LList)result.getValue()));
    }

    public static Runnable createRunnableAndInvocable( Procedure procedure, Object... args) {
        return new InvokablyRunnable( InvokableSchemeProcedure.createSecretarillyInvokable( procedure ), args );
    }
}
