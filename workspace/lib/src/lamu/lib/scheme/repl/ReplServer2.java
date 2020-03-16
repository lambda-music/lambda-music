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
import lamu.lib.stream.SisoReceiver;
import lamu.lib.stream.SisoReceiverListener;
import lamu.lib.stream.SisoReceiverServiceListener;

/**
 * An older version of {@link ReplServer}. This version adopts the
 * multi-buffer model; it keeps the key name of the current buffer and the
 * incoming stream data will go to the current buffer. And these buffers are
 * stored in a map. Later it appeared that this system is ineffective.
 * 
 * The new version of ReplSisoListener adopts the single-buffer model; it holds
 * only one buffer and the incoming stream data will go into the buffer. There
 * are defined operations on the buffer such as exec, load and save. These
 * operation resets the current buffer.
 * 
 * For further information, see the new {@link ReplServer}.
 */
public class ReplServer2 implements SisoReceiverListener, SisoReceiverServiceListener {
    public static final String ERROR_COMMAND = "error";
    public static final String DEFAULT_BUFFER_KEY = "default";
    public static final String NULL_BUFFER_KEY = "null";
    public static final String DEFAULT_COMMAND_STRING = "show";

    protected final SchemeEngine schemeEngine;
    protected String prefix = ";lamu:";
    public ReplServer2( SchemeEngine schemeEngine ) {
        super();
        this.schemeEngine = schemeEngine;
    }
    public ReplServer2( SchemeEngine schemeEngine, String prefix ) {
        super();
        this.schemeEngine = schemeEngine;
        this.prefix = prefix;
    }

    final HashMap<String,StringBuffer> bufferMap = new HashMap<>();
    String currentBufferKey;
    List<Thread> threadList = Collections.synchronizedList( new ArrayList<>() );

    void initBufferMap() {
        // Reset the null buffer to empty.
        bufferMap.put( NULL_BUFFER_KEY, new StringBuffer() );

        // Reset the default buffer.
        if ( bufferMap.containsKey( DEFAULT_BUFFER_KEY ) ) {
            bufferMap.get( DEFAULT_BUFFER_KEY ).setLength(0);
        } else {
            bufferMap.put( DEFAULT_BUFFER_KEY, new StringBuffer() );
        }
        
//        // Reset the current buffer key to the default.
//        if ( bufferMap.containsKey( currentBufferKey )) {
//            currentBufferKey = DEFAULT_BUFFER_KEY;
//        }
    }

    public String filterKey(String key) {
        if ( key == null || "".equals( key ) ) {
            key = currentBufferKey;
        }
        return key;
    }

    public StringBuffer getBuffer() {
        return getBuffer( "" );
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
    public StringBuffer getBuffer( String key ) {
        key = filterKey(key);

        if ( key.equals( NULL_BUFFER_KEY )) {
            return new StringBuffer();
        } else if ( bufferMap.containsKey(key)) {
            return bufferMap.get( key );
        } else {
            StringBuffer value = new StringBuffer();
            bufferMap.put( key, value );
            return value;
        }
    }
    public boolean hasBuffer( String key ) {
        key = filterKey(key);
        return bufferMap.containsKey(key);
    }

    public boolean clearBuffer( String key ) {
        key = filterKey(key);
        if ( bufferMap.containsKey(key)) {
            getBuffer(key).setLength(0);
            return true;
        } else {
            return false;
        }
    }
    public boolean deleteBuffer( String key ) {
        key = filterKey(key);
        Object result = bufferMap.remove(key);
        initBufferMap();
        return result != null;
    }

    public String currentBufferKey() {
        return currentBufferKey;
    }
    public String currentBufferKey( String key ) {
        if ( key == null || "".equals( key ) ) {
            key = DEFAULT_BUFFER_KEY;
        }
        this.currentBufferKey = key;
        return key;
    }
    
    private static Pattern ESCAPE_DOUBLE_QUOTATIONS = Pattern.compile( "\"" );
    public static String escapeDoubleQuotation( String s ) {
        return ESCAPE_DOUBLE_QUOTATIONS.matcher(s).replaceAll( "\\\\\"" );
    }

    {
        initBufferMap();
        currentBufferKey( DEFAULT_BUFFER_KEY );
    }

    public void hello(SisoReceiver receiver) {
        receiver.postMessage( SisoReceiver.createPrintMessage( "; ======================================" ));
        receiver.postMessage( SisoReceiver.createPrintMessage( "; ======= LAMU-REPL PREPROCESSOR =======" ));
        receiver.postMessage( SisoReceiver.createPrintMessage( "; ======================================" ));
        receiver.postMessage( 
            SisoReceiver.createPrintMessage( 
                String.format( 
                      "; Now Lamu-REPL Preprocessor is ready.\n"
                    + "; For further information, type %s\n"
                    , prefix + "help" )));
        receiver.postMessage( SisoReceiver.createPrintMessage( 
            String.format( 
                  "; Please note that all commands need these leading characters.\n"
                + "; The current leading characters are:\n"
                + "; %s\n"
                + "; You can change the leading characters by %s command."
                , prefix
                , "prefix" )));
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

    final HashMap<String,SisoReceiverListener> commandMap = new HashMap<String, SisoReceiverListener>();
    {
        commandMap.put( ERROR_COMMAND, new SisoReceiverListener() {
            @Override
            public void process(SisoReceiver receiver, String s) {
                receiver.postMessage( 
                    SisoReceiver.createPrintMessage(
                        "'((status . failed)\n (message-type . comment)\n ( message . \"unknown command\" ))"));
            }
        });
        commandMap.put("quit", new SisoReceiverListener() {
            @Override
            public void process(SisoReceiver receiver, String s) {
                receiver.postMessage( SisoReceiver.createQuitMessage() );
            }
        });
        commandMap.put("bye", new SisoReceiverListener() {
            @Override
            public void process(SisoReceiver receiver, String s) {
                receiver.postMessage( SisoReceiver.createPrintMessage( "'((status . succeeded)\n (message-type . comment)\n (message . bye))" ));
                receiver.postMessage( SisoReceiver.createQuitMessage() );
            }
        });
        commandMap.put("alive?", new SisoReceiverListener() {
            @Override
            public void process(SisoReceiver receiver, String s) {
                receiver.postMessage( SisoReceiver.createPrintMessage("'((status . succeeded)\n (message-type . result)\n (message . #t))" ));
            }
        });
        commandMap.put("hello", new SisoReceiverListener() {
            @Override
            public void process(SisoReceiver receiver, String s) {
                receiver.postMessage( SisoReceiver.createPrintMessage("'((status . succeeded)\n (message-type . comment)\n (message . hello! ))" ));
            }
        });
        commandMap.put("hello?", new SisoReceiverListener() {
            @Override
            public void process(SisoReceiver receiver, String s) {
                hello( receiver );
                receiver.postMessage( SisoReceiver.createPrintMessage("'((status . succeeded)\n (message-type . comment)\n (message . hello!!! ))"));
            }
        });
        /**
         * Set the current buffer to the specified buffer.
         * If no argument is specified, it sets to the default buffer. 
         */
        commandMap.put( "select", new SisoReceiverListener() {
            @Override
            public void process(SisoReceiver receiver, String s) {
                s = s.trim();
                currentBufferKey(s);
                receiver.postMessage( 
                    SisoReceiver.createPrintMessage(
                        String.format(
                            "'((status . succeeded)\n (message-type . current-buffer-key)\n (message . \"%s\" ))", s )));
            }
        });
        commandMap.put( DEFAULT_COMMAND_STRING, new SisoReceiverListener() {
            @Override
            public void process(SisoReceiver receiver, String s) {
                StringBuffer buffer = getBuffer( s );
                receiver.postMessage( 
                    SisoReceiver.createPrintMessage(
                        String.format( 
                            "'((status . succeeded )\n"
                          + "  (message-type . 'buffer-content)\n"
                          + "  (message . \""
                          + "%s\" )))" 
                          , escapeDoubleQuotation( buffer.toString() ) )
                        ));
            }
        });

        commandMap.put( "list", new SisoReceiverListener() {
            @Override
            public void process(SisoReceiver receiver, String s) {
                String str = sortJoin( " ", bufferMap.keySet() );
                receiver.postMessage( 
                    SisoReceiver.createPrintMessage( 
                        String.format( 
                            "'((status . succeeded )\n"
                          + "  (message-type . 'buffer-list)\n"
                          + "  (message . ("
                          + "%s)))" 
                          , str )));
            }
        });

        /**
         * Set the current buffer to the specified buffer.
         * If no argument is specified, returns the current buffer key. 
         */
        commandMap.put( "buffer", new SisoReceiverListener() {
            @Override
            public void process(SisoReceiver receiver, String s) {
                s=s.trim();
                String result;
                if ( s.equals("")) {
                    result = currentBufferKey();
                } else {
                    result = currentBufferKey(s);
                }
                receiver.postMessage( 
                    SisoReceiver.createPrintMessage( 
                        String.format( "'((status . succeeded)\n (message-type . current-buffer-key)\n (message . %s))", result ) ));
            }
        });

        commandMap.put( "clear", new SisoReceiverListener() {
            @Override
            public void process(SisoReceiver receiver, String s) {
                s=s.trim();
                boolean result = deleteBuffer( s );
                receiver.postMessage( 
                    SisoReceiver.createPrintMessage(
                        String.format(
                            "'((status . succeeded)\n (message-type . exists)\n (message . %s))"
                            , result ? "#t" : "#f" )));
            }
        });

        commandMap.put( "delete", new SisoReceiverListener() {
            @Override
            public void process(SisoReceiver receiver, String s) {
                s=s.trim();
                boolean result = deleteBuffer( s );
                receiver.postMessage( 
                    SisoReceiver.createPrintMessage(
                        String.format(
                            "'((status . succeeded)\n (message-type . exists)\n (message . %s))"
                            , result ? "#t" : "#f" )));
            }
        });
        commandMap.put( "exec", new SisoReceiverListener() {
            @Override
            public void process( SisoReceiver receiver, String s ) {
                StringBuffer buffer = getBuffer( s.trim() );
                String script = buffer.toString();

                EvaluatorReceiver resultReceiver = new EvaluatorReceiver() {
                    @Override
                    public void receive(String schemeScript, SchemeResult schemeResult) {
                        receiver.postMessage( 
                            SisoReceiver.createPrintMessage( schemeResult.getValueAsString()));
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


        commandMap.put( "help", new SisoReceiverListener() {
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

        commandMap.put( "prefix", new SisoReceiverListener() {
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
            commandMap.get( "exec" ).process( receiver, getBuffer().toString() );
            commandMap.get( "quit" ).process( receiver, null  );
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
            if ("".equals( commandString )) {
               commandString = DEFAULT_COMMAND_STRING; 
            }
            
            if ( commandMap.containsKey( commandString ) ) {
                commandMap.get( commandString ).process( receiver, paramString );
            } else {
                commandMap.get( ERROR_COMMAND ).process( receiver, paramString );
            }
        } else {
            getBuffer().append( s ).append( "\n" );
        }
    }

    public static void main(String[] args) {
        SchemeEngine schemeEngine = new SchemeEngine();
        schemeEngine.requestInit();
        new SisoReceiver( null, System.in, System.out, new ReplServer2( schemeEngine, ";" ) ).requestInit();
    }
}
