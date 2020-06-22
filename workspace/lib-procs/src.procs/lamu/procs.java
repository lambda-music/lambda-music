package lamu;

import java.io.File;
import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.logging.Level;

import gnu.mapping.Environment;
import gnu.mapping.Values;
import lamu.lib.helps.LamuDocument;
import lamu.lib.kawautils.SchemeValues;
import lamu.lib.kawautils.procedures.MultipleNamedProcedure1;
import lamu.lib.kawautils.procedures.MultipleNamedProcedureN;
import lamu.lib.log.Logger;
import lamu.lib.procs.PulsarProcessWrapper;

public class procs implements Runnable {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE,   msg, e   ); }
    static void logInfo (String msg             ) { LOGGER.log(Level.INFO,     msg      ); }
    static void logWarn (String msg             ) { LOGGER.log(Level.WARNING,  msg      ); }

    public static final LamuDocument createProcessBean = null;

    public static final String NAMEDARG_DIRECTORY = "dir";
    public static final CreateProcessProc createProcessProc = new CreateProcessProc(new String[] { "create-process", "newp" });
    
    /**
     * See {@link lamu.lib.evaluators.SchemeEvaluatorUtils#useResolve(File)}
     * The `dir:` named parameter specifies the current directory. Note that this parameter is not easily
     * determined unless using the special values of SchemeEvaluator : <code>#!current-dir</code> / <code>#!current-file</code>
     * For further information, see SchemeEvaluator.
     */
    public static final class CreateProcessProc extends MultipleNamedProcedureN {
        public CreateProcessProc(String[] names) {
            super(names);
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            HashMap<String,Object> namedArgs = new HashMap<>();
            ArrayList<Object> plainArgs = new ArrayList<>();
            SchemeValues.parseArguments(args, namedArgs, plainArgs);
            
   //                List l =  new ArrayList( Arrays.asList( args ) );
   //                l.add( 0, Keyword.make( "directory" ) );
   //                l.add( 1, new File( System.getProperty( "user.dir" ) ) );
   //                args = l.toArray();
            
            List<String> stringPlainArgs = SchemeValues.toStringList( plainArgs );
            ProcessBuilder sb = new ProcessBuilder( stringPlainArgs );

            // The new way to set the current directory. (Sun, 03 May 2020 02:35:09 +0900)
            if ( namedArgs.containsKey(NAMEDARG_DIRECTORY)) {
                sb.directory( new File(SchemeValues.toString( namedArgs.get(NAMEDARG_DIRECTORY))).getCanonicalFile());
            } else {
                /*
                 * This should be very useful but I am afraid of side-effects; therefore, leave it disabled.  
                 * (Tue, 23 Jun 2020 02:14:30 +0900) >>>
                 * // sb.directory( SchemeEvaluator.getCurrentBaseFile().getParentFile() );
                 * (Tue, 23 Jun 2020 02:14:30 +0900) <<<
                 */
            }
            
//            // XXX ??? (Tue, 24 Mar 2020 06:09:27 +0900) <<< This should be integrated.
//            sb.directory( ((Path) Shell.currentLoadPath.get()).toFile() );
            
            // TODO the IO should be able to controlled. this is bogus.
            // REMOVED (Tue, 24 Mar 2020 05:20:12 +0900) >>>
            // sb.inheritIO();
            // REMOVED (Tue, 24 Mar 2020 05:20:12 +0900) <<<
            return new PulsarProcessWrapper( sb.start(), new ArrayList( stringPlainArgs ) );
        }
    }

    public static final LamuDocument destroyProcessBean = null;

    public static final DestroyProcessProc destroyProcessProc = new DestroyProcessProc(new String[] { "destroy-process", "kilp" });
    public static final class DestroyProcessProc extends MultipleNamedProcedureN {
        public DestroyProcessProc(String[] names) {
            super(names);
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            for ( int i=0; i<args.length; i++ ) {
                if ( args[i] instanceof Process ) {
                    ((Process)args[i]).destroy();
                } else if ( args[i] instanceof PulsarProcessWrapper ) {
                        ((PulsarProcessWrapper)args[i]).destroy();
                } else {
                    logWarn( "warning : the value of the arguments no " + i + " is not a process object." );
                }
            }
            return Values.empty;
        }
    }

    public static final LamuDocument killProcessBean = null;

    public static final KillProcessProc killProcessProc = new KillProcessProc(new String[] { "kill-process", "fkilp" });
    public static final class KillProcessProc extends MultipleNamedProcedureN {
        public KillProcessProc(String[] names) {
            super(names);
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            for ( int i=0; i<args.length; i++ ) {
                if ( args[i] instanceof Process ) {
                    ((Process)args[i]).destroyForcibly();
                } else if ( args[i] instanceof PulsarProcessWrapper ) {
                    ((PulsarProcessWrapper)args[i]).destroyForcibly();
                } else {
                    logWarn( "warning : the value of the arguments no " + i + " is not a process object." );
                }
            }
            return Values.empty;
        }
    }

    public static final LamuDocument sleepBean = null;

    public static final SleepProc sleepProc = new SleepProc(new String[] { "sleep" });
    public static final class SleepProc extends MultipleNamedProcedure1 {
        public SleepProc(String[] names) {
            super(names);
        }

        @Override
        public Object apply1(Object arg1) throws Throwable {
            Thread.sleep( SchemeValues.toInteger( arg1 ));
            return Values.empty;
        }
    }

    
    
    /**
     * Initializes an environment of scheme engine and defines API for the scripts.
     * 
     * @param scheme
     *            the scheme instance to initialize.
     */
    public static void initScheme( Environment env ) {
        SchemeValues.defineLambda( env, createProcessProc);
        SchemeValues.defineLambda( env, destroyProcessProc); 
        SchemeValues.defineLambda( env, killProcessProc);
        SchemeValues.defineLambda( env, sleepProc );
    }

    @Override
    public void run() {
        initScheme( Environment.getCurrent() );
    }

    
}
