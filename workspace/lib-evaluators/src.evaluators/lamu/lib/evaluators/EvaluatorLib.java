package lamu.lib.evaluators;

import java.io.File;
import java.io.Reader;
import java.util.Arrays;

import gnu.mapping.Environment;
import kawa.standard.Scheme;
import lamu.lib.kawautils.SchemeValues;
import lamu.lib.kawautils.procedures.MultipleNamedProcedure0;
import lamu.lib.kawautils.procedures.MultipleNamedProcedure1;

public class EvaluatorLib {
    public static final MultipleNamedProcedure0 currentEvaluatorProc = new CurrentEvaluatorProc(new String[] { "current-evaluator" });
    public static final class CurrentEvaluatorProc extends MultipleNamedProcedure0 {
        public CurrentEvaluatorProc(String[] names) {
            super(names);
        }

        @Override
        public Object apply0() throws Throwable {
            return MultiplexEvaluator.getCurrent();
        }
    }

    public static final MultipleNamedProcedure0 isCurrentEvaluatorPresentProc = new IsCurrentEvaluatorPresentProc(new String[] { "current-evaluator-present?" });
    public static final class IsCurrentEvaluatorPresentProc extends MultipleNamedProcedure0 {
        public IsCurrentEvaluatorPresentProc(String[] names) {
            super(names);
        }

        @Override
        public Object apply0() throws Throwable {
            return MultiplexEvaluator.isPresent();
        }
    }
    
    static File toFile(Object arg) {
        return new File( SchemeValues.toString(arg));
    }

    public static final ResolveUseProc useResolve = new ResolveUseProc(new String[] { "use-resolve" });
    public static final class ResolveUseProc extends MultipleNamedProcedure1 {
        public ResolveUseProc(String[] names) {
            super(names);
        }

        @Override
        public Object apply1( Object arg ) throws Throwable {
            return 
                SchemeValues.toSchemeString(
                    SchemeEvaluatorUtils.useResolve(
                        toFile(arg)).getPath());
        }

    }

    public static final UseProc use = new UseProc(new String[] { "use" });
    public static final class UseProc extends MultipleNamedProcedure1 {
        public UseProc(String[] names) {
            super(names);
        }

        @Override
        public Object apply1( Object arg ) throws Throwable {
            return SchemeEvaluatorUtils.use( toFile(arg));
        }
    }
    public static final UseReadProc useRead = new UseReadProc(new String[] { "use-read" });
    public static final class UseReadProc extends MultipleNamedProcedure1 {
        public UseReadProc(String[] names) {
            super(names);
        }

        @Override
        public Object apply1( Object arg ) throws Throwable {
            return SchemeEvaluatorUtils.useRead(toFile(arg));
        }
    }


    //////////////////////////////////////////////////////////////////////////////////////////
    // Scheme Initializer 
    //////////////////////////////////////////////////////////////////////////////////////////
    /**
     * <blockquote>
     * "loadRelative" was moved from 
     * {@link SchemeEvaluatorImplementation#evaluateScheme(Scheme, Runnable, Reader, File, File, String)}
     * 
     * Integrated with SchemeEngineLib class (Wed, 29 Apr 2020 23:29:13 +0900)
     * </blockquote>
     */
    public static void initScheme( Environment env ) {
        SchemeValues.defineLambda(env, currentEvaluatorProc  );
        SchemeValues.defineLambda(env, isCurrentEvaluatorPresentProc );
        SchemeValues.defineVar( env, kawa.standard.load.loadRelative, Arrays.asList( "source" ) );
        SchemeValues.defineLambda( env, useResolve );
        SchemeValues.defineLambda( env, use );
        SchemeValues.defineLambda( env, useRead );
    }
}
