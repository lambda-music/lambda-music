package lamu.utils;

import gnu.mapping.Environment;
import gnu.mapping.Procedure;
import gnu.mapping.Values;
import gnu.mapping.WrongArguments;
import lamu.lib.doc.LamuDocument;
import lamu.lib.evaluators.InvokableSchemeProcedure;
import lamu.lib.evaluators.SchemeUtils;
import lamu.lib.scheme.proc.MultipleNamedProcedure0;
import lamu.lib.scheme.proc.MultipleNamedProcedureN;
import lamu.utils.lib.PulsarGuiUtils;
import lamu.utils.lib.PulsarSharedTimer;

public class gui implements Runnable {
    public static final String DOCS_ID = "kawapad-gui-procedures";
        
    public static final MakeTimerProc makeTimerProc = new MakeTimerProc(new String[] { "schedule", "make-timer" });
    public static final class MakeTimerProc extends MultipleNamedProcedureN {
        public MakeTimerProc(String[] names) {
            super(names);
        }

        public Object apply2(Object arg1, Object arg2) {
            Runnable runnable = PulsarSharedTimer.createTimer( null/*TODO (Sun, 12 Apr 2020 20:43:01 +0900)*/ , 
                SchemeUtils.toInteger( arg1 ), 
                -1, 
                InvokableSchemeProcedure.createSecretarillyInvokable( (Procedure)arg2 ) );
            
            return new MultipleNamedProcedure0() {
                public Object apply0() throws Throwable {
                    runnable.run();
                    return Values.empty;
                };
            };
        }

        @Override
        public Object apply3(Object arg0, Object arg1,Object arg2 ) throws Throwable {
            Runnable runnable = PulsarSharedTimer.createTimer( null/*TODO (Sun, 12 Apr 2020 20:43:01 +0900)*/, 
                SchemeUtils.toInteger( arg0 ), 
                SchemeUtils.toInteger( arg1 ), 
                InvokableSchemeProcedure.createSecretarillyInvokable( (Procedure)arg2 ) );
   
            return new MultipleNamedProcedure0() {
                public Object apply0() throws Throwable {
                    runnable.run();
                    return Values.empty;
                };
            };
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            if ( args.length == 2 ) {
                return apply2( args[0], args[1] );
            } else if ( args.length == 3 ) {
                return apply3( args[0], args[1], args[2] );
            } else {
                WrongArguments.checkArgCount( "schedure", 2, 3, args.length );
                return false;
            }
        }
    }

    public static final MakeTimerBean makeTimerBean = new MakeTimerBean();
    public static final class MakeTimerBean extends LamuDocument {
        {
            setCategory( DOCS_ID );
            setNames("make-timer" );
            setParameterDescription( "delay interval proc" );
            addParameter( 0, "delay",     "number",    null, false , "" );
            addParameter( 0, "interval",  "number",    null, false , "" );
            addParameter( 0, "callback",  "procedure", null, false , "" );
        
            setReturnValueDescription( "::procedure" );
            setShortDescription( "||<name/>|| creates a new timer object. " );
            setLongDescription( ""
                    + "This procedure registers the specified procedure as a callback procedure of the timer; "
                    + "the procedure will be called with the specified period and with the specified delay. "
                    + "The return value is a cancel procedure. When the cancel procedure is called, the timer stops calling the "
                    + "callback procedure. "
                    + "" 
            );
        }
    }

    @Override
    public void run() {
        PulsarGuiUtils.initScheme( Environment.getCurrent() );
    }
}
