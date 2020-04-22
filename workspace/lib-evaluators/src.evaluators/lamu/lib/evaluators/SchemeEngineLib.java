package lamu.lib.evaluators;

import gnu.mapping.Environment;
import lamu.lib.kawautils.SchemeValues;
import lamu.lib.kawautils.procedures.MultipleNamedProcedure0;

public class SchemeEngineLib {
    public static final MultipleNamedProcedure0 currentSchemeEngine = new CurrentSchemeEngine(new String[] { "current-evaluator" });
    public static final class CurrentSchemeEngine extends MultipleNamedProcedure0 {
        public CurrentSchemeEngine(String[] names) {
            super(names);
        }

        @Override
        public Object apply0() throws Throwable {
            return MultiplexEvaluator.getCurrent();
        }
    }

    public static final MultipleNamedProcedure0 isCurrentSchemeEnginePresent = new IsCurrentSchemeEnginePresent(new String[] { "current-evaluator-present?" });
    public static final class IsCurrentSchemeEnginePresent extends MultipleNamedProcedure0 {
        public IsCurrentSchemeEnginePresent(String[] names) {
            super(names);
        }

        @Override
        public Object apply0() throws Throwable {
            return MultiplexEvaluator.isPresent();
        }
    }
    public static void initScheme( Environment env ) {
        SchemeValues.defineLambda(env, currentSchemeEngine  );
        SchemeValues.defineLambda(env, isCurrentSchemeEnginePresent );
    }

}
