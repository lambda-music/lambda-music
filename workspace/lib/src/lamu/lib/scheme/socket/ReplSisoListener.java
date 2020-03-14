package lamu.lib.scheme.socket;

import java.util.HashMap;

import lamu.lib.scheme.SchemeEngine;
import lamu.lib.scheme.SchemeResult;
import lamu.lib.scheme.socket.SisoReceiver.SisoListener;

public class ReplSisoListener implements SisoListener {
    public static final String ERROR_COMMAND = "error";
    public static final String DEFAULT_BUFFER_KEY = "default";
    public static final String NULL_BUFFER_KEY = "null";

    protected final SchemeEngine schemeEngine;
    protected String prefix = ";;;";
    public ReplSisoListener(SchemeEngine schemeEngine ) {
        super();
        this.schemeEngine = schemeEngine;
    }
    class ExecuteScheme implements Runnable {
        final SisoReceiver server;
        final String script;
        public ExecuteScheme(SisoReceiver server, String script) {
            super();
            this.server = server;
            this.script = script;
        }

        @Override
        public void run() {
            SchemeResult result = 
                    schemeEngine.getEvaluatorManager().getCurrentEvaluator().evaluate( server.getThreadInitializerCollection(), script, "console" );
            server.postMessage( SisoReceiver.createPrintMessage( result.getValueAsString() ));
        }
    }

    final HashMap<String,StringBuffer> bufferMap = new HashMap<>();
    StringBuffer currentBuffer;
    String currentBufferKey;
    public StringBuffer getBuffer( String key ) {
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
    public StringBuffer getCurrentBuffer() {
        return currentBuffer;
    }
    public String getCurrentBufferKey() {
        return currentBufferKey;
    }
    public void clearCurrentBuffer() {
        getCurrentBuffer().setLength(0);
    }

    public StringBuffer selectCurrentBuffer( String key ) {
        this.currentBuffer = getBuffer( key );
        this.currentBufferKey = key;
        return this.currentBuffer;
    }
    {
        selectCurrentBuffer( DEFAULT_BUFFER_KEY );
    }

    final HashMap<String,SisoListener> commandMap = new HashMap<String, SisoReceiver.SisoListener>();

    {
        commandMap.put( ERROR_COMMAND, new SisoListener() {
            @Override
            public void process(SisoReceiver server, String s) {
                server.postMessage( SisoReceiver.createPrintMessage("'error"));
            }
        });
        commandMap.put("quit", new SisoListener() {
            @Override
            public void process(SisoReceiver server, String s) {
                server.postMessage( SisoReceiver.createQuitMessage() );
            }
        });
        commandMap.put("bye", new SisoListener() {
            @Override
            public void process(SisoReceiver server, String s) {
                server.postMessage( SisoReceiver.createPrintMessage( "'bye" ));
                server.postMessage( SisoReceiver.createQuitMessage() );
            }
        });
        commandMap.put("alive?", new SisoListener() {
            @Override
            public void process(SisoReceiver server, String s) {
                server.postMessage( SisoReceiver.createPrintMessage("#t"));
            }
        });
        commandMap.put("hello", new SisoListener() {
            @Override
            public void process(SisoReceiver server, String s) {
                server.postMessage( SisoReceiver.createPrintMessage("hello"));
            }
        });
        commandMap.put("hello?", new SisoListener() {
            @Override
            public void process(SisoReceiver server, String s) {
                server.postMessage( SisoReceiver.createPrintMessage("hello!"));
            }
        });
        commandMap.put("select-buffer", new SisoListener() {
            @Override
            public void process(SisoReceiver server, String s) {
                StringBuffer currentBuffer = selectCurrentBuffer(s);
                server.postMessage( SisoReceiver.createPrintMessage(
                        String.format(
                                "'(( length . %d ))" ,
                                currentBuffer.toString().split("\n").length )));
            }
        });
        commandMap.put( "select", commandMap.get( "select-buffer" ));
        commandMap.put( "show-buffer", new SisoListener() {
            @Override
            public void process(SisoReceiver server, String s) {
                StringBuffer currentBuffer = getCurrentBuffer();
                server.postMessage( 
                        SisoReceiver.createPrintMessage( currentBuffer.toString() ));
            }
        });
        commandMap.put( "show", commandMap.get( "show-buffer" ));
        commandMap.put( "clear-buffer", new SisoListener() {
            @Override
            public void process(SisoReceiver server, String s) {
                clearCurrentBuffer();
                server.postMessage( 
                        SisoReceiver.createPrintMessage( "#t" ));
            }
        });
        commandMap.put( "clear", commandMap.get( "clear-buffer" ));
        commandMap.put( "execute-buffer", new SisoListener() {
            @Override
            public void process( SisoReceiver server, String s ) {
                StringBuffer currentBuffer = getCurrentBuffer();

                Thread t = new Thread( createExecutor(
                        server,
                        currentBuffer.toString(),
                        "scheme-socket-server." + getCurrentBufferKey() 
                        ));
                t.setDaemon(false);
                t.start();
            }

            private Runnable createExecutor(SisoReceiver server, String script, String scriptURI ) {
                return new Runnable() {
                    @Override
                    public void run() {
                        SchemeResult result = schemeEngine.getEvaluatorManager().getCurrentEvaluator().evaluate( script, scriptURI );
                        server.postMessage( 
                                SisoReceiver.createPrintMessage( result.getValueAsString() ));
                    }
                };
            }
        });
        commandMap.put( "execute", commandMap.get( "execute-buffer" ));
    }


    @Override
    public void process( SisoReceiver server, String s ) {
        if ( s == null ) {
            commandMap.get( "execute" ).process( server, getCurrentBuffer().toString() );
            commandMap.get( "quit" ).process( server, null  );
            // end the process
        } else if ( s.startsWith(prefix)) {
            String commandString = s.substring( prefix.length() ).trim();
            String paramString = "";
            int i = commandString.indexOf( " " );
            if ( 0<=i ) {
                paramString = commandString.substring(i+1).trim();
                commandString = commandString.substring(0,i).trim();
            }
            if ( commandMap.containsKey( commandString ) ) {
                commandMap.get( commandString ).process( server, paramString );
            } else {
                commandMap.get( ERROR_COMMAND ).process( server, paramString );
            }
        } else {
            getCurrentBuffer().append( s ).append( "\n" );
        }
    }
    public static void main(String[] args) {
        SchemeEngine schemeEngine = new SchemeEngine();
        schemeEngine.requestInit();
        new SisoReceiver( null, System.in, System.out, new ReplSisoListener( schemeEngine ) ).requestInit();
    }
}
