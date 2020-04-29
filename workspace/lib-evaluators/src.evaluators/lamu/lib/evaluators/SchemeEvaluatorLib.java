package lamu.lib.evaluators;

import gnu.expr.Language;
import gnu.mapping.Environment;
import lamu.lib.kawautils.SchemeValues;
import lamu.lib.kawautils.procedures.MultipleNamedProcedure0;

public class SchemeEvaluatorLib {
    public static final MultipleNamedProcedure0 currentEnvironment = new CurrentEnvironmentProc(new String[] { "current-environment" });
    public static final class CurrentEnvironmentProc extends MultipleNamedProcedure0 {
        public CurrentEnvironmentProc(String[] names) {
            super(names);
        }

        @Override
        public Object apply0() throws Throwable {
            return Language.getDefaultLanguage();
        }
    }

    public static final MultipleNamedProcedure0 currentScheme = new CurrentSchemeProc(new String[] { "current-scheme" });
    public static final class CurrentSchemeProc extends MultipleNamedProcedure0 {
        public CurrentSchemeProc(String[] names) {
            super(names);
        }

        @Override
        public Object apply0() throws Throwable {
            return Language.getDefaultLanguage();
        }
    }
    public static void initScheme( Environment env ) {
        SchemeValues.defineLambda( env, currentScheme );
        SchemeValues.defineLambda( env, currentEnvironment );
    }
}
