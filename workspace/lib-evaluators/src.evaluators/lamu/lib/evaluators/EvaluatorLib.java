package lamu.lib.evaluators;

import java.io.File;
import java.io.Reader;
import java.util.Arrays;

import gnu.expr.Language;
import gnu.lists.LList;
import gnu.mapping.Environment;
import gnu.mapping.Procedure;
import gnu.mapping.ProcedureN;
import gnu.mapping.Values;
import kawa.standard.Scheme;
import lamu.lib.kawautils.SchemeValues;
import lamu.lib.kawautils.procedures.MultipleNamedProcedure0;
import lamu.lib.kawautils.procedures.MultipleNamedProcedure0or1;
import lamu.lib.kawautils.procedures.MultipleNamedProcedure1;
import lamu.lib.threads.LamuThreadLocalInitializer;

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

    /**
	 * The `tether` makes the specified procedure to thread-independent. When a
	 * thread creates a procedure and passes the procedure to another routine which
	 * is occationally executed by other threads, the passed procedure is usually
	 * not able to be executed correctly because those settings which are stored in
	 * {@link ThreadLocal} are lost when the procedure comes to another thread.
	 * Espacially the lost of the two critical setting in Kawa:
	 * {@link Language#getDefaultLanguage()} and {@link Environment#getCurrent()} is
	 * very problematic.
	 * <p/>
	 * This procedure wraps the specified procedure by another procedure which
	 * restores the current {@link ThreadLocal} settings. The wrapped procedure is
	 * thread-independent; therefore, it can be safely executed in other threads.
	 * <p/>
	 * This `tether` procedure creates a snapshot of the settings stored in the
	 * {@link ThreadLocal} (which is at least known to this Evaluator) of the
	 * current thread, then creates a wrapper procedure to restore the snapshot.
	 * <p/>
	 * If `tether` procedure is called with no argument, the result procedure simply
	 * restores the created snapshots.
	 */
    public static final TetherProc tetherProc = new TetherProc(new String[] { "tether" });
    private static final class TetheredProcedure extends ProcedureN {
		private final Procedure procedure;
		private final LamuThreadLocalInitializer initializer;
		private final Environment env;
		private final Language lang;
		private TetheredProcedure(String name, Procedure procedure) {
			super(name);
			this.procedure = procedure;
			this.initializer = new LamuThreadLocalInitializer();
			this.env = Environment.getCurrent();
			this.lang = Language.getDefaultLanguage();
		}
		@Override
		public Object applyN(Object[] args) throws Throwable {
			initializer.restore();
			Environment.setCurrent(env);
			Language.setCurrentLanguage(lang);
			if ( procedure == null ) {
				return Values.empty;
			} else {
				return procedure.applyN(args);
			}
		}
	}
    public static final class TetherProc extends MultipleNamedProcedure0or1 {
		public TetherProc(String[] names) {
            super(names);
        }
		@Override
		public Object apply0() throws Throwable {
			return new TetheredProcedure( "tethered", null );
		}
        @Override
        public Object apply1( Object arg ) throws Throwable {
        	Procedure procedure = (Procedure) arg;
			String name = procedure.getName();
			if ( name == null ) name = "procedure";
			return new TetheredProcedure("tethered-" + name, procedure);
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
        SchemeValues.defineLambda( env, tetherProc );
    }
}
