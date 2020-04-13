package lamu.lib.scheme;

import java.io.File;
import java.io.Reader;

import gnu.expr.Language;
import gnu.mapping.Environment;
import kawa.standard.Scheme;
import lamu.lib.scheme.proc.MultipleNamedProcedure0;

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

    //////////////////////////////////////////////////////////////////////////////////////////
    // Scheme Initializer 
    //////////////////////////////////////////////////////////////////////////////////////////
    /**
     * "loadRelative" was moved from 
     * {@link SchemeEvaluatorImplementation#evaluateScheme(Scheme, Runnable, Reader, File, File, String)}  
     */
    public static void initScheme( Scheme scheme ) {
        Environment env = scheme.getEnvironment();
        SchemeUtils.defineLambda(env, currentScheme);
        SchemeUtils.defineLambda(env, currentEnvironment);

        //        SchemeUtils.defineVar(env, new Procedure0() {
        //            @Override
        //            public Object apply0() throws Throwable {
        //                SchemeUtils.getAllKey( (Scheme)Language.getDefaultLanguage() );
        //                return 
        //            }
        //        }, "get-all-identifiers" );
    }

}
