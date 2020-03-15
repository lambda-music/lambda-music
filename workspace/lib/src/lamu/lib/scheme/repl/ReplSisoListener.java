package lamu.lib.scheme.repl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.regex.Pattern;

import lamu.lib.scheme.EvaluatorReceiver;
import lamu.lib.scheme.SchemeEngine;
import lamu.lib.scheme.SchemeResult;

public class ReplSisoListener implements SisoProcessor, SisoListener {
    public static final String ERROR_COMMAND = "error";
    public static final String DEFAULT_COMMAND_STRING = "echo";
    private static final String DEFAULT_BUFFER = ".";

    protected final SchemeEngine schemeEngine;
    protected String prefix = ";lamu:";
    public ReplSisoListener( SchemeEngine schemeEngine ) {
        super();
        this.schemeEngine = schemeEngine;
    }
    public ReplSisoListener( SchemeEngine schemeEngine, String prefix ) {
        super();
        this.schemeEngine = schemeEngine;
        this.prefix = prefix;
    }

    final HashMap<String,String> bufferMap = new HashMap<>();
    final StringBuffer buffer = new StringBuffer();
    final List<Thread> threadList = Collections.synchronizedList( new ArrayList<>() );

    private String filterKey(String key) {
        if ( "".equals( key ) ) {
            key = DEFAULT_BUFFER;
        }
        return key;
    }

    /**
     * returns the specified buffer.
     * 
     * "null" to return the null-buffer. The null buffer cannot store values and it always returns 
     * a null-string when it is evaluated.
     *  
     * @param key
     *    the key of the buffer to be get; if it is null or a null-string value, return the current buffer.  
     * @return
     *    the {@link StringBuffer } object corresponding to the specified key.
     */
    public String getBuffer( String key ) {
        key = filterKey(key);
        return bufferMap.get(key); 
    }
    public boolean hasBuffer( String key ) {
        key = filterKey(key);
        return bufferMap.containsKey(key);
    }
    public boolean deleteBuffer( String key ) {
        key = filterKey(key);
        Object result = bufferMap.remove(key);
        return result != null;
    }
    public boolean loadBuffer( String key ) {
        key = filterKey(key);
        if ( bufferMap.containsKey(key)) {
            String s = bufferMap.get(key);
            this.buffer.setLength(0);
            this.buffer.append(s);
            return true;
        } else {
            return false;
        }
    }
    public boolean saveBuffer( String key ) {
        key = filterKey(key);
        bufferMap.put( key, this.buffer.toString());
        this.buffer.setLength(0);
        return true;
    }

    public void clearCurrentBuffer() {
        saveBuffer( DEFAULT_BUFFER );
        this.buffer.setLength(0);
    }
    public String getCurrentBuffer() {
        return ReplSisoListener.this.buffer.toString();
    }
    public void appendCurrentBuffer( String s ) {
        ReplSisoListener.this.buffer.append( s );
    }

    public static String doubleQuote( String s ) {
        return "\"" + escapeDoubleQuotation(s) + "\"";
    }
    private static Pattern ESCAPE_DOUBLE_QUOTATIONS = Pattern.compile( "\"" );
    public static String escapeDoubleQuotation( String s ) {
        return ESCAPE_DOUBLE_QUOTATIONS.matcher(s).replaceAll( "\\\\\"" );
    }
    public void hello(SisoReceiver receiver) {
        receiver.postMessage( SisoReceiver.createPrintMessage(
            "; ======================================\n" + 
            "; ======= LAMU-REPL PREPROCESSOR =======\n" +
            "; ======================================\n" +
            ";"  ));
        
        receiver.postMessage( SisoReceiver.createPrintMessage( 
            String.format( 
                "; Now Lamu-REPL Preprocessor is ready.\n" +
                "; For further information, type %s\n" +
                ";",
                prefix + "help"
                )));

        receiver.postMessage( SisoReceiver.createPrintMessage( 
            String.format("; # About Prefix #\n" +
                          "; Please note that all commands need these leading characters.\n" +
                          "; The current leading characters are: '%s' (without the quote characters.)\n" +
                          "; You can change the leading characters by '%s%s' command.\n" +
                          ";",
                prefix, prefix, "prefix")));

        receiver.postMessage( SisoReceiver.createPrintMessage( 
            String.format( 
                "; # About Execution on EOF #\n" +
                "; Please note that it executes the data on the current stream\n" +
                "; when EOF is arrived on the input stream.\n" +
                "; If this behavior is not desirable, execute '%s%s' before the EOF.\n"
                , prefix , "clear" )));

    }
    
    static String sortJoin( String delimitor, Collection<String> collection ) {
        ArrayList<String> list = new ArrayList<>( collection );
        list.sort( new Comparator<String>() {
            @Override
            public int compare(String o1, String o2) {
                return o1.compareTo(o2);
            }
        });
        return String.join( delimitor, list );
    }
    
    static String createMessage( String status, String messageType, String message ) {
        return String.format( 
            "'((status . %s )\n"
                + "  (message-type . %s)\n"
                + "  (message . %s )))"
                , escapeDoubleQuotation( status )
                , escapeDoubleQuotation( messageType ) 
                ,  message );
    }

    final HashMap<String,SisoProcessor> commandMap = new HashMap<String, SisoProcessor>();
    {
        commandMap.put( ERROR_COMMAND, new SisoProcessor() {
            @Override
            public void process(SisoReceiver receiver, String s) {
                receiver.postMessage( 
                    SisoReceiver.createPrintMessage(
                        "'((status . failed)\n (message-type . reason)\n ( message . \"unknown command\" ))"));
            }
        });
        commandMap.put("quit", new SisoProcessor() {
            @Override
            public void process(SisoReceiver receiver, String s) {
                receiver.postMessage( SisoReceiver.createQuitMessage() );
            }
        });
        commandMap.put("bye", new SisoProcessor() {
            @Override
            public void process(SisoReceiver receiver, String s) {
                receiver.postMessage( SisoReceiver.createPrintMessage( "'((status . succeeded)\n (message-type . comment)\n (message . bye))" ));
                receiver.postMessage( SisoReceiver.createQuitMessage() );
            }
        });
        commandMap.put("alive?", new SisoProcessor() {
            @Override
            public void process(SisoReceiver receiver, String s) {
                receiver.postMessage( SisoReceiver.createPrintMessage("'((status . succeeded)\n (message-type . result)\n (message . #t))" ));
            }
        });
        commandMap.put("hello", new SisoProcessor() {
            @Override
            public void process(SisoReceiver receiver, String s) {
                receiver.postMessage( SisoReceiver.createPrintMessage("'((status . succeeded)\n (message-type . comment)\n (message . hello! ))" ));
            }
        });
        commandMap.put("hello!", new SisoProcessor() {
            @Override
            public void process(SisoReceiver receiver, String s) {
                hello( receiver );
                receiver.postMessage( SisoReceiver.createPrintMessage("'((status . succeeded)\n (message-type . comment)\n (message . hello!!! ))"));
            }
        });
        
        commandMap.put( "echo", new SisoProcessor() {
            @Override
            public void process(SisoReceiver receiver, String s) {
                s = s.trim();
                String content = getCurrentBuffer();
                clearCurrentBuffer();
                receiver.postMessage( 
                    SisoReceiver.createPrintMessage(
                        createMessage( "succeeded"
                            , "buffer-content"
                            , "\""+escapeDoubleQuotation( content ) + "\"" )));
            }
        });

        commandMap.put( "show", new SisoProcessor() {
            @Override
            public void process(SisoReceiver receiver, String s) {
                s = s.trim();
                String content;
                if ( "".equals( s ) ) {
                    content = getCurrentBuffer();
                } else {
                    if (hasBuffer(s)) {
                        content = getBuffer(s);
                    } else {
                        content = null;
                    }
                }
                
                if ( content != null ) {
                    receiver.postMessage( 
                        SisoReceiver.createPrintMessage(
                            createMessage( "succeeded"
                                , "buffer-content"
                                , "\""+escapeDoubleQuotation( content ) + "\"" )));
                            
                } else {
                    receiver.postMessage( 
                        SisoReceiver.createPrintMessage(
                            createMessage( "failed"
                                , "reason"
                                , "specified-buffer-does-not-exist" )));
                }
            }
        });

        commandMap.put( "list", new SisoProcessor() {
            @Override
            public void process(SisoReceiver receiver, String s) {
                String str = sortJoin( " ", bufferMap.keySet() );
                receiver.postMessage( 
                    SisoReceiver.createPrintMessage( 
                        String.format( 
                            "'((status . succeeded )\n"
                          + "  (message-type . 'available-buffer-list)\n"
                          + "  (message . ("
                          + "%s)))" 
                          , str )));
            }
        });

        commandMap.put( "clear", new SisoProcessor() {
            @Override
            public void process(SisoReceiver receiver, String s) {
                clearCurrentBuffer();
                boolean result=true;
                receiver.postMessage( 
                    SisoReceiver.createPrintMessage(
                        createMessage( 
                            "succeeded" , "exists?", result ? "#t" : "#f" )));
            }
        });
        commandMap.put( "save", new SisoProcessor() {
            @Override
            public void process(SisoReceiver receiver, String s) {
                s=s.trim();
                boolean result = saveBuffer( s );
                receiver.postMessage( 
                    SisoReceiver.createPrintMessage(
                        createMessage( 
                            "succeeded" , "exists?", result ? "#t" : "#f" )));
            }
        });
        commandMap.put( "load", new SisoProcessor() {
            @Override
            public void process(SisoReceiver receiver, String s) {
                s=s.trim();
                boolean result = loadBuffer( s );
                receiver.postMessage( 
                    SisoReceiver.createPrintMessage(
                        createMessage( 
                            "succeeded" , "exists?", result ? "#t" : "#f" )));
            }
        });

        commandMap.put( "delete", new SisoProcessor() {
            @Override
            public void process(SisoReceiver receiver, String s) {
                s=s.trim();
                boolean result = deleteBuffer( s );
                receiver.postMessage( 
                    SisoReceiver.createPrintMessage(
                        createMessage( 
                            "succeeded" , "exists?", result ? "#t" : "#f" )));
            }
        });
        commandMap.put( "exec", new SisoProcessor() {
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
                                "succeeded" , "empty", "'ignored-empty-script" )));
                    return;
                }
                
                EvaluatorReceiver resultReceiver = new EvaluatorReceiver() {
                    @Override
                    public void receive(String schemeScript, SchemeResult schemeResult) {
                        receiver.postMessage( 
                            SisoReceiver.createPrintMessage(
                                createMessage(
                                    schemeResult.isSucceeded() ? "succeeded" : "failed" ,
                                        "sexpr",
                                        schemeResult.isEmpty() ? "'()" : schemeResult.getValueAsString()
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


        commandMap.put( "help", new SisoProcessor() {
            @Override
            public void process(SisoReceiver receiver, String s) {
                String str = sortJoin( " ", commandMap.keySet() );
                receiver.postMessage( 
                    SisoReceiver.createPrintMessage( 
                        String.format( 
                              "'((status . succeeded )\n"
                            + "  (message-type . 'available-commands)\n"
                            + "  (message . ("
                            + "%s)))" 
                            , str )));
            }
        });
        commandMap.put( "?", commandMap.get( "help" ));

        commandMap.put( "prefix", new SisoProcessor() {
            @Override
            public void process(SisoReceiver receiver, String s) {
                s = s.trim();
                if ( "".equals( s )) {
                    //
                } else {
                    prefix = s;
                }
                receiver.postMessage( 
                    SisoReceiver.createPrintMessage( 
                        String.format( 
                            "'((status . succeeded)\n (message-type . prefix)\n (message . \"%s\"))"
                            , prefix )));
            }
        });
    }
    @Override
    public void start(SisoReceiver receiver ) {
        hello(receiver);
    }
    @Override
    public void end(SisoReceiver receiver) {
    }

    @Override
    public void process( SisoReceiver receiver, String s ) {
        if ( s == null ) {
            // Execute the current buffer when the stream is terminated.
            commandMap.get( "exec" ).process( receiver, "" );
            commandMap.get( "quit" ).process( receiver, ""  );
            // end the process
            return;
        } else if ( s.startsWith(prefix)) {
            String commandString = s.substring( prefix.length() ).trim();
            String paramString = "";
            int i = commandString.indexOf( " " );
            if ( 0<=i ) {
                paramString = commandString.substring(i+1).trim();
                commandString = commandString.substring(0,i).trim();
            }
            
//            if ( "".equals( commandString )) {
//               commandString = DEFAULT_COMMAND_STRING; 
//            }
            
            if ( commandMap.containsKey( commandString ) ) {
                commandMap.get( commandString ).process( receiver, paramString );
            } else {
                commandMap.get( ERROR_COMMAND ).process( receiver, paramString );
            }
        } else {
            appendCurrentBuffer( s );
        }
    }

    public static void main(String[] args) {
        SchemeEngine schemeEngine = new SchemeEngine();
        schemeEngine.requestInit();
        new SisoReceiver( null, System.in, System.out, new ReplSisoListener( schemeEngine, ";" ) ).requestInit();
    }
}
