package lamu.lib.evaluators;

import java.util.ArrayList;
import java.util.List;

import gnu.lists.LList;
import lamu.lib.kawautils.SchemeValues;

public class EvaluatorUtils {
    public static List<String> getAllKey( Evaluator evaluator ) {
        SchemeResult result = evaluator.evaluate( 
            "(environment-fold (interaction-environment) cons '())", "get-all" );
        result.throwIfError();
        return new ArrayList<>( SchemeValues.toStringList((LList)result.getValue()));
    }
}
