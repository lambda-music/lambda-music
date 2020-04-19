package lamu.lib.scheme.repl;

import java.util.regex.Pattern;

import lamu.lib.scheme.AsyncEvaluator;
import lamu.lib.scheme.AsyncThreadManager;
import lamu.lib.scheme.EvaluatorReceiver;
import lamu.lib.scheme.MultiplexEvaluator;
import lamu.lib.scheme.SchemeResult;
import lamu.lib.scheme.ThreadManager;
import lamu.lib.stream.SisoReceiver;
import lamu.lib.stream.SisoReceiverListener;
import lamu.lib.stream.StdioStream;

public class ReplServer extends ReplClientServer {
    protected final ThreadManager threadManager;
    protected final MultiplexEvaluator multiplexEvaluator;
    public ReplServer( ThreadManager threadManager, String prefix, MultiplexEvaluator multiplexEvaluator ) {
        super( prefix );
        this.threadManager = threadManager;
        this.multiplexEvaluator = multiplexEvaluator;
    }
    public ReplServer( ThreadManager threadManager, MultiplexEvaluator multiplexEvaluator ) {
        super();
        this.threadManager = threadManager;
        this.multiplexEvaluator = multiplexEvaluator;
    }

    public void hello( SisoReceiver receiver ) {
        StringBuilder message = new StringBuilder();

        message.append( String.format(
            "; ======================================\n" + 
            "; ======= LAMU-REPL PREPROCESSOR =======\n" +
            "; ======================================\n" +
            ";" ) + "\n" );

        message.append( String.format( 
            "; Now Lamu-REPL Preprocessor is ready.\n" +
            "; For further information, type %s\n" +
            ";", prefix + "help" ) + "\n" );

        message.append( String.format( 
            "; # About Prefix #\n" +
            "; Please note that all commands need these leading characters.\n" +
            "; The current leading characters are: '%s' (without the quote characters.)\n" +
            "; You can change the leading characters by '%s%s' command.\n" +
            ";", prefix, prefix, "prefix" ) + "\n" );

        message.append( String.format(
            "; # About Execution on EOF #\n" +
            "; Please note that when the processor detect EOF on the input stream, \n" +
            "; it executes the data on the current buffer.\n" +
            "; If this behavior is not desirable, execute '%s%s' before EOF is sent.\n"
            , prefix , "clear" ));

        // put prefix on every line.
        String result = Pattern.compile( "^", Pattern.MULTILINE ).matcher( message.toString() ).replaceAll( prefix );

        receiver.postMessage( SisoReceiver.createPrintMessage( result ));
    }

    @Override
    public void notifyParent(SisoReceiver receiver) {
    }

    @Override
    public void start(SisoReceiver receiver ) {
        hello( receiver );
    }
    @Override
    public void end(SisoReceiver receiver) {
    }
    
    {
        registerCommand("hello", new SisoReceiverListener() {
            @Override
            public void process(SisoReceiver receiver, String s) {
                receiver.postMessage( 
                    SisoReceiver.createPrintMessage(
                        createMessage( 
                            fetchCurrentSession()
                            , "succeeded"
                            , "comment", "hello!" )));
            }
        });
        registerCommand("hello!", new SisoReceiverListener() {
            @Override
            public void process(SisoReceiver receiver, String s) {
                hello( receiver );
                receiver.postMessage( 
                    SisoReceiver.createPrintMessage(
                        createMessage( 
                            fetchCurrentSession()
                            , "succeeded"
                            , "comment", "hello!!!" )));
            }
        });

        registerCommand( "exec", new SisoReceiverListener() {
            @Override
            public void process( SisoReceiver receiver, String s ) {
//                s=s.trim();
//                loadBuffer(s);
                
                String script = getCurrentBuffer().trim();
                clearCurrentBuffer();

                String currentSession = fetchCurrentSession();
                
                if ( "".equals( script )) {
                    receiver.postMessage( 
                        SisoReceiver.createPrintMessage(
                            createMessage( 
                                currentSession , "succeeded", "empty", "'ignored-empty-script" )));
                    return;
                }
                
                EvaluatorReceiver resultReceiver = new EvaluatorReceiver() {
                    @Override
                    public void receive(SchemeResult schemeResult) {
                        receiver.postMessage( 
                            SisoReceiver.createPrintMessage(
                                createMessage(
                                    currentSession ,
                                        schemeResult.isSucceeded() ? "succeeded" : "failed",
                                        "sexpr", schemeResult.isEmpty() ? "'()" : schemeResult.getValueAsString()
                                    )));
                    }
                };

                AsyncEvaluator.executeAsync(
                    threadManager, 
                    receiver.getThreadInitializerCollection(), 
                    script, 
                    multiplexEvaluator, 
                    resultReceiver, 
                    null, 
                    null, 
                    "console(repl)" );
            }
        });
    }

    @Override
    protected void onReplTerminate( SisoReceiver receiver, String s ) {
        // Execute the current buffer when the stream is terminated.
        callCommand( "exec" , receiver, "" );
//        callCommand( "quit" , receiver, "" );
    }


    public static void main(String[] args) {
        ThreadManager threadManager = new AsyncThreadManager();
        MultiplexEvaluator multiplexEvaluator = MultiplexEvaluator.createLocal();
        multiplexEvaluator.requestInit();
        new SisoReceiver( StdioStream.INSTANCE, new ReplServer( threadManager, ";", multiplexEvaluator ) ).requestInit();
    }
}
