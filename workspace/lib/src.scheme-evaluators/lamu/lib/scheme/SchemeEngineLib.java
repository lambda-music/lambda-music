package lamu.lib.scheme;

import gnu.mapping.Environment;
import lamu.lib.scheme.proc.MultipleNamedProcedure0;

public class SchemeEngineLib {
    public static final MultipleNamedProcedure0 currentSchemeEngine = new CurrentSchemeEngine(new String[] { "current-scheme-engine" });
    public static final class CurrentSchemeEngine extends MultipleNamedProcedure0 {
        public CurrentSchemeEngine(String[] names) {
            super(names);
        }

        @Override
        public Object apply0() throws Throwable {
            return SchemeEngine.getCurrent();
        }
    }

    public static final MultipleNamedProcedure0 isCurrentSchemeEnginePresent = new IsCurrentSchemeEnginePresent(new String[] { "current-scheme-engine-present?" });
    public static final class IsCurrentSchemeEnginePresent extends MultipleNamedProcedure0 {
        public IsCurrentSchemeEnginePresent(String[] names) {
            super(names);
        }

        @Override
        public Object apply0() throws Throwable {
            return SchemeEngine.isPresent();
        }
    }
    public static void initScheme( Environment env ) {
        SchemeUtils.defineLambda(env, currentSchemeEngine  );
        SchemeUtils.defineLambda(env, isCurrentSchemeEnginePresent );
    }

}
