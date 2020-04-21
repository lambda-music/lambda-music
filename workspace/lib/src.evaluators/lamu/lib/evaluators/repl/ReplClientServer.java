package lamu.lib.evaluators.repl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.regex.Pattern;

import lamu.lib.MersenneTwisterFast;
import lamu.lib.stream.SisoReceiver;
import lamu.lib.stream.SisoReceiverListener;
import lamu.lib.stream.SisoReceiverServiceListener;

public abstract class ReplClientServer implements SisoReceiverListener, SisoReceiverServiceListener {
    public static final String ERROR_COMMAND = "error";
    private static final String DEFAULT_BUFFER = ".";
    private static final String RESULT_MESSAGE = "done";
    private static final String DEFAULT_PREFIX = ";lamu:";
    private static final String APPEND_COMMAND = "append";

    protected String prefix = DEFAULT_PREFIX;

    protected final MersenneTwisterFast random = new MersenneTwisterFast( new int[] { 
        ((int)(Math.random() * Integer.MAX_VALUE)) << (int) System.currentTimeMillis(),
        ((int)(Math.random() * Integer.MAX_VALUE)) << (int) System.currentTimeMillis(),
        ((int)(Math.random() * Integer.MAX_VALUE)) << (int) System.currentTimeMillis(),
        ((int)(Math.random() * Integer.MAX_VALUE)) << (int) System.currentTimeMillis(),
        0xbcd, 0xcde, 0xdef, 0xabc, 
    });

    public ReplClientServer() {
        super();
        this.prefix = DEFAULT_PREFIX;
    }
    public ReplClientServer( String prefix ) {
        super();
        this.prefix = prefix;
    }
    public String getPrefix() {
        return prefix;
    }

    String currentSession = null;
    String fetchCurrentSession() {
        String s = this.currentSession;
        this.currentSession = null;
        return s;
    }
    public String getCurrentSession() {
        return currentSession;
    }
    void setCurrentSession(String currentSession) {
        this.currentSession = currentSession;
    }

    final HashMap<String,String> bufferMap = new HashMap<>();
    final StringBuffer buffer = new StringBuffer();

    private String filterKey(String key) {
        if ( "".equals( key ) ) {
            key = DEFAULT_BUFFER;
        }
        return key;
    }

    protected String escapeText( String s ) {
        return escapeText( this.prefix, s ); 
    }

    static String escapeText( String prefix, String s ) {
        Pattern ESCAPE = Pattern.compile( "^(" + Pattern.quote( prefix ) + ".*)$", Pattern.MULTILINE );
        return ESCAPE.matcher( s ).replaceAll( prefix.replaceAll( "\\$", "\\\\\\$" ) +  APPEND_COMMAND+ "$1" );
    }
    
    protected String createCommandString( String commandName, String parameter ) {
        if ( parameter == null || parameter.trim().equals("") ) {
            return this.prefix + commandName;
        } else {
            return this.prefix + commandName + " " + parameter;
        }
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
        return ReplClientServer.this.buffer.toString();
    }
    public void appendCurrentBuffer( String s ) {
        ReplClientServer.this.buffer.append( s ).append("\n");
    }

    public static String doubleQuote( String s ) {
        return "\"" + escapeDoubleQuotation(s) + "\"";
    }
    private static Pattern ESCAPE_DOUBLE_QUOTATIONS = Pattern.compile( "\"" );
    public static String escapeDoubleQuotation( String s ) {
        return ESCAPE_DOUBLE_QUOTATIONS.matcher(s).replaceAll( "\\\\\"" );
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
    
    protected String createMessage( String currentSession, String status, String messageType, String message ) {
        StringBuilder sb = new StringBuilder();
        if ( currentSession != null ) {
            sb.append( createCommandString( "session", currentSession ) ).append( "\n" );
        }

        sb.append( String.format( 
            "((status . %s )\n"
                + "  (message-type . %s)\n"
                + "  (message . %s )))"
                , escapeDoubleQuotation( status )
                , escapeDoubleQuotation( messageType ) 
                ,  message )).append("\n");
        
        sb.append( createCommandString( RESULT_MESSAGE , null ) ).append("\n");

        return sb.toString(); 
    }
    
    protected final HashMap<String,SisoReceiverListener> commandMap = new HashMap<String, SisoReceiverListener>();
    public void registerCommand( String commandName ,SisoReceiverListener listener ) {
        commandMap.put( commandName, listener );
    }
    protected boolean callCommand( String commandName, SisoReceiver receiver, String paramString ) {
        if ( commandMap.containsKey( commandName ) ) {
            commandMap.get( commandName ).process( receiver, paramString );
            return true;
        } else {
            return false;
        }
    }

    {
        registerCommand( ERROR_COMMAND, new SisoReceiverListener() {
            @Override
            public void process(SisoReceiver receiver, String s) {
                s  = s.trim();
                if ( "".equals(s)) {
                    s = "unknown command";
                }
                
                receiver.postMessage( 
                    SisoReceiver.createPrintMessage(
                        createMessage( 
                            fetchCurrentSession()
                            , "failed"
                            , "reason", "\""+escapeDoubleQuotation( s ) + "\"" )));
            }
        });
        registerCommand("quit", new SisoReceiverListener() {
            @Override
            public void process(SisoReceiver receiver, String s) {
                receiver.postMessage( SisoReceiver.createQuitMessage() );
            }
        });
        registerCommand("bye", new SisoReceiverListener() {
            @Override
            public void process(SisoReceiver receiver, String s) {
                receiver.postMessage( 
                    SisoReceiver.createPrintMessage(
                        createMessage( 
                            fetchCurrentSession() , "succeeded" , "result", "bye" )));
                receiver.postMessage( SisoReceiver.createQuitMessage() );
            }
        });
        registerCommand("alive?", new SisoReceiverListener() {
            @Override
            public void process(SisoReceiver receiver, String s) {
                receiver.postMessage( 
                    SisoReceiver.createPrintMessage(
                        createMessage( 
                            fetchCurrentSession()
                            , "succeeded"
                            , "result", "#t" )));
            }
        });
        registerCommand("session", new SisoReceiverListener() {
            @Override
            public void process(SisoReceiver receiver, String s) {
                s = s.trim();
                if ( "".equals(s) ) {
                } else {
                    setCurrentSession(s);
                }

                // Maybe this should not return any value to the peer.
                if ( false ) {
                    String session = getCurrentSession();
                    if ( session == null ) {
                        receiver.postMessage( 
                            SisoReceiver.createPrintMessage(
                                createMessage( 
                                    session
                                    , "succeeded"
                                    , "session-id", "#f" )));
                    } else {                    
                        receiver.postMessage( 
                            SisoReceiver.createPrintMessage(
                                createMessage( 
                                    session
                                    , "succeeded"
                                    , "session-id", "\""+escapeDoubleQuotation( session ) + "\"" )));
                    }
                }
            }
        });

        registerCommand( "help", new SisoReceiverListener() {
            @Override
            public void process(SisoReceiver receiver, String s) {
                String str = sortJoin( " ", commandMap.keySet() );
                receiver.postMessage( 
                    SisoReceiver.createPrintMessage(
                        createMessage(
                            fetchCurrentSession() ,
                                "succeeded",
                                "available-commands",
                                "(" + str + ")" )));
            }
        });
        registerCommand( "?", commandMap.get( "help" ));

        registerCommand( "prefix", new SisoReceiverListener() {
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

        registerCommand( "append", new SisoReceiverListener() {
            @Override
            public void process(SisoReceiver receiver, String s) {
                s = s.trim();
                appendCurrentBuffer( s );
            }
        });

        // ==============================================
        
        registerCommand( "echo", new SisoReceiverListener() {
            @Override
            public void process(SisoReceiver receiver, String s) {
                s = s.trim();
                String content = getCurrentBuffer();
                clearCurrentBuffer();
                receiver.postMessage( 
                    SisoReceiver.createPrintMessage(
                        createMessage( 
                            fetchCurrentSession()
                            , "succeeded"
                            , "buffer-content", "\""+escapeDoubleQuotation( content ) + "\"" )));
            }
        });

        registerCommand( "show", new SisoReceiverListener() {
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
                            createMessage( 
                                fetchCurrentSession()
                                , "succeeded"
                                , "buffer-content", "\""+escapeDoubleQuotation( content ) + "\"" )));
                            
                } else {
                    receiver.postMessage( 
                        SisoReceiver.createPrintMessage(
                            createMessage(
                                fetchCurrentSession()
                                , "failed"
                                , "reason", "specified-buffer-does-not-exist" )));
                }
            }
        });

        registerCommand( "list", new SisoReceiverListener() {
            @Override
            public void process(SisoReceiver receiver, String s) {
                String str = sortJoin( " ", bufferMap.keySet() );
                receiver.postMessage( 
                    SisoReceiver.createPrintMessage( 
                        createMessage(
                            fetchCurrentSession()
                            , "succeeded"
                            , "available-buffer-list",
                            "(" + str +  ")" )
                        ));
            }
        });

        registerCommand( "clear", new SisoReceiverListener() {
            @Override
            public void process(SisoReceiver receiver, String s) {
                clearCurrentBuffer();
                boolean result=true;
                receiver.postMessage( 
                    SisoReceiver.createPrintMessage(
                        createMessage( 
                            fetchCurrentSession() , "succeeded", "exists?", result ? "#t" : "#f" )));
            }
        });
        registerCommand( "save", new SisoReceiverListener() {
            @Override
            public void process(SisoReceiver receiver, String s) {
                s=s.trim();
                boolean result = saveBuffer( s );
                receiver.postMessage( 
                    SisoReceiver.createPrintMessage(
                        createMessage( 
                            fetchCurrentSession() , "succeeded", "exists?", result ? "#t" : "#f" )));
            }
        });
        registerCommand( "load", new SisoReceiverListener() {
            @Override
            public void process(SisoReceiver receiver, String s) {
                s=s.trim();
                boolean result = loadBuffer( s );
                receiver.postMessage( 
                    SisoReceiver.createPrintMessage(
                        createMessage( 
                            fetchCurrentSession() , "succeeded", "exists?", result ? "#t" : "#f" )));
            }
        });

        registerCommand( "delete", new SisoReceiverListener() {
            @Override
            public void process(SisoReceiver receiver, String s) {
                s=s.trim();
                boolean result = deleteBuffer( s );
                receiver.postMessage( 
                    SisoReceiver.createPrintMessage(
                        createMessage( 
                            fetchCurrentSession() , "succeeded", "exists?", result ? "#t" : "#f" )));
            }
        });
    }
    
    protected abstract void onReplTerminate(SisoReceiver receiver, String paramString);
    

    @Override
    public void process( SisoReceiver receiver, String s ) {
        if ( s == null ) {
            onReplTerminate( receiver, s );
            return;
        } else if ( s.startsWith( prefix )) {
            String commandName = s.substring( prefix.length() ).trim();
            String paramString = "";

            // (Tue, 17 Mar 2020 02:35:48 +0900)
            // if the command string starts with ; , treat it as a comment.
            if ( commandName.startsWith( ";" )) {
                // do nothing
                return;
            }
            
            int i = commandName.indexOf( " " );
            if ( 0<=i ) {
                paramString = commandName.substring(i+1).trim();
                commandName = commandName.substring(0,i).trim();
            }
            
//            if ( "".equals( commandString )) {
//               commandString = DEFAULT_COMMAND_STRING; 
//            }

            if ( ! callCommand( commandName, receiver, paramString) ) {
                if ( ! callCommand( ERROR_COMMAND, receiver, "unknown-command (" + commandName + ")" ) ) {;
                    throw new Error("internal error " + ERROR_COMMAND + " was not found" );
                }
            }
        } else {
            appendCurrentBuffer( s );
        }
    }
    public static void main2(String[] args) {
        String s =
            "hello\n" +
                "hello\n" +
                "f$o$o:hello\n" +
                "hello\n" +
                "hello\n" +
        "";
        System.out.println( escapeText( "f$o$o:", s ) );
    }
}
