package lamu.lib.scheme.repl;

import lamu.lib.scheme.EvaluatorReceiver;
import lamu.lib.scheme.SchemeEngine;
import lamu.lib.scheme.SchemeResult;
import lamu.lib.stream.SisoReceiver;
import lamu.lib.stream.SisoReceiverListener;

public class ReplServer extends ReplClientServerBasic {
    protected final SchemeEngine schemeEngine;
    public ReplServer( String prefix, SchemeEngine schemeEngine ) {
        super( prefix );
        this.schemeEngine = schemeEngine;
    }
    public ReplServer( SchemeEngine schemeEngine ) {
        super();
        this.schemeEngine = schemeEngine;
    }

    @Override
    public void start(SisoReceiver receiver ) {
        hello( receiver );
    }
    @Override
    public void end(SisoReceiver receiver) {
    }
    
    {
        registerCommand( "exec", new SisoReceiverListener() {
            @Override
            public void process( SisoReceiver receiver, String s ) {
//                s=s.trim();
//                loadBuffer(s);
                
                String script = getCurrentBuffer().trim();
                clearCurrentBuffer();

                if ( "".equals( script )) {
                    receiver.postMessage( 
                        SisoReceiver.createPrintMessage(
                            createMessage( 
                                fetchCurrentSession() , "succeeded", "empty", "'ignored-empty-script" )));
                    return;
                }
                
                EvaluatorReceiver resultReceiver = new EvaluatorReceiver() {
                    @Override
                    public void receive(SchemeResult schemeResult) {
                        receiver.postMessage( 
                            SisoReceiver.createPrintMessage(
                                createMessage(
                                    fetchCurrentSession() ,
                                        schemeResult.isSucceeded() ? "succeeded" : "failed",
                                        "sexpr", schemeResult.isEmpty() ? "'()" : schemeResult.getValueAsString()
                                    )));
                    }
                };

                Runnable evaluationRunner = SchemeEngine.createEvaluationRunner(
                    receiver.getThreadInitializerCollection(), script,
                    schemeEngine.getEvaluatorManager().getCurrentEvaluator(),
                    resultReceiver, null , null, "console(repl)" );

                Thread t = new Thread( new Runnable() {
                    @Override
                    public void run() {
                        try {
                            evaluationRunner.run();
                        } finally {
                            threadList.remove( Thread.currentThread() );
                        }
                    }
                });
                threadList.add( t );
                t.start();
            }
        });
    }

    @Override
    protected void onTerminate( SisoReceiver receiver, String s ) {
        // Execute the current buffer when the stream is terminated.
        callCommand( "exec" , receiver, "" );
        callCommand( "quit" , receiver, "" );
    }


    public static void main(String[] args) {
        SchemeEngine schemeEngine = new SchemeEngine();
        schemeEngine.requestInit();
        new SisoReceiver( null, System.in, System.out, new ReplServer( ";", schemeEngine ) ).requestInit();
    }
}
