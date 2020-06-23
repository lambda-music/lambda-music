package lamu.lib.evaluators;

import java.io.File;
import java.io.Reader;
import java.util.Arrays;

import gnu.lists.LList;
import gnu.mapping.Environment;
import gnu.mapping.Values;
import kawa.standard.Scheme;
import lamu.lib.kawautils.SchemeValues;
import lamu.lib.kawautils.procedures.MultipleNamedProcedure0;
import lamu.lib.kawautils.procedures.MultipleNamedProcedure0or1;
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

    public static final CurrentThreadManagerProc currentThreadManager = new CurrentThreadManagerProc( new String[] { "current-thread-manager" });
    public static final class CurrentThreadManagerProc extends MultipleNamedProcedure1 {
        public CurrentThreadManagerProc(String[] names) {
            super(names);
        }
        @Override
        public Object apply1( Object arg ) throws Throwable {
            return ((AsyncThreadManager)ThreadManager.getCurrent()); 
        }
    }

    public static final CurrentThreadsProc currentThreads = new CurrentThreadsProc(new String[] { "current-threads" });
    public static final class CurrentThreadsProc extends MultipleNamedProcedure1 {
        public CurrentThreadsProc(String[] names) {
            super(names);
        }

        @Override
        public Object apply1( Object arg ) throws Throwable {
            return
                LList.makeList(
                    ((AsyncThreadManager)ThreadManager.getCurrent()).getThreads()); 
        }
    }

    /**
     * `abort` procedure aborts the currently executing code and passes the
     * specified value as a result of the evaluation. If no argument is specified,
     * the result of the evaluation will be {@link Values#empty}.
     * <p/>
     * For further information, see the source code of 
     * {@link SchemeEvaluatorImplementation#evaluateSchemeProc(Scheme, Runnable, Reader, File, String)} method.
     */
    public static final AbortEvaluatorProc abortEvaluatorProc = new AbortEvaluatorProc(new String[] { "abort" });
    public static final class AbortEvaluatorProc extends MultipleNamedProcedure0or1 {
        public AbortEvaluatorProc(String[] names) {
            super(names);
        }
        @Override
        public Object apply0() throws Throwable {
            throw new EvaluatorAborted();
        }
        @Override
        public Object apply1( Object arg ) throws Throwable {
            throw new EvaluatorAborted(arg);
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
        SchemeValues.defineLambda( env, currentThreads );
        SchemeValues.defineLambda( env, abortEvaluatorProc );
    }
}
