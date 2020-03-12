package quartz.lib.scheme.socket;

import java.util.HashMap;

import quartz.lib.scheme.SchemeEngine;
import quartz.lib.scheme.SchemeResult;
import quartz.lib.scheme.socket.SisoServer.SisoListener;
import quartz.lib.thread.ThreadInitializerCollection;

public class SchemeSocketServer implements SisoListener {
	public static final String ERROR_COMMAND = "error";
	public static final String DEFAULT_BUFFER_KEY = "default";
	public static final String NULL_BUFFER_KEY = "null";
	
	protected final SchemeEngine schemeEngine;
	protected final ThreadInitializerCollection initializerCollection;
	protected String prefix = ";;;";
	public SchemeSocketServer(SchemeEngine schemeEngine, ThreadInitializerCollection initializerCollection ) {
		super();
		this.schemeEngine = schemeEngine;
		this.initializerCollection = initializerCollection;
		this.schemeEngine.requestInit();
	}
	class ExecuteScheme implements Runnable {
		final SisoServer server;
		final String script;
		public ExecuteScheme(SisoServer server, String script) {
			super();
			this.server = server;
			this.script = script;
		}

		@Override
		public void run() {
			SchemeResult result = 
					schemeEngine.getEvaluatorManager().getCurrentEvaluator().evaluate( initializerCollection, script, "console" );
			server.postMessage( SisoServer.createPrintMessage( result.getValueAsString() ));
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

	final HashMap<String,SisoListener> commandMap = new HashMap<String, SisoServer.SisoListener>();
	
	{
		commandMap.put( ERROR_COMMAND, new SisoListener() {
			@Override
			public void process(SisoServer server, String s) {
				server.postMessage( SisoServer.createPrintMessage("'error"));
			}
		});
		commandMap.put("quit", new SisoListener() {
			@Override
			public void process(SisoServer server, String s) {
				server.postMessage( SisoServer.createQuitMessage() );
			}
		});
		commandMap.put("bye", new SisoListener() {
			@Override
			public void process(SisoServer server, String s) {
				server.postMessage( SisoServer.createPrintMessage( "'bye" ));
				server.postMessage( SisoServer.createQuitMessage() );
			}
		});
		commandMap.put("alive?", new SisoListener() {
			@Override
			public void process(SisoServer server, String s) {
				server.postMessage( SisoServer.createPrintMessage("#t"));
			}
		});
		commandMap.put("hello", new SisoListener() {
			@Override
			public void process(SisoServer server, String s) {
				server.postMessage( SisoServer.createPrintMessage("hello"));
			}
		});
		commandMap.put("hello?", new SisoListener() {
			@Override
			public void process(SisoServer server, String s) {
				server.postMessage( SisoServer.createPrintMessage("hello!"));
			}
		});
		commandMap.put("select-buffer", new SisoListener() {
			@Override
			public void process(SisoServer server, String s) {
				StringBuffer currentBuffer = selectCurrentBuffer(s);
				server.postMessage( SisoServer.createPrintMessage(
						String.format(
								"'(( length . %d ))" ,
								currentBuffer.toString().split("\n").length )));
			}
		});
		commandMap.put( "select", commandMap.get( "select-buffer" ));
		commandMap.put( "show-buffer", new SisoListener() {
			@Override
			public void process(SisoServer server, String s) {
				StringBuffer currentBuffer = getCurrentBuffer();
				server.postMessage( 
						SisoServer.createPrintMessage( currentBuffer.toString() ));
			}
		});
		commandMap.put( "show", commandMap.get( "show-buffer" ));
		commandMap.put( "clear-buffer", new SisoListener() {
			@Override
			public void process(SisoServer server, String s) {
				clearCurrentBuffer();
				server.postMessage( 
						SisoServer.createPrintMessage( "#t" ));
			}
		});
		commandMap.put( "clear", commandMap.get( "clear-buffer" ));
		commandMap.put( "execute-buffer", new SisoListener() {
			@Override
			public void process( SisoServer server, String s ) {
				StringBuffer currentBuffer = getCurrentBuffer();
				
				Thread t = new Thread( createExecutor(
						server,
						currentBuffer.toString(),
						"scheme-socket-server." + getCurrentBufferKey() 
						));
				t.setDaemon(false);
				t.start();
			}

			private Runnable createExecutor(SisoServer server, String script, String scriptURI ) {
				return new Runnable() {
					@Override
					public void run() {
						SchemeResult result = schemeEngine.getEvaluatorManager().getCurrentEvaluator().evaluate( script, scriptURI );
						server.postMessage( 
								SisoServer.createPrintMessage( result.getValueAsString() ));
					}
				};
			}
		});
		commandMap.put( "execute", commandMap.get( "execute-buffer" ));
	}

	
	@Override
	public void process( SisoServer server, String s ) {
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
		ThreadInitializerCollection ic = new ThreadInitializerCollection( "hello", null );
		new SisoServer( null, null, System.in, System.out, new SchemeSocketServer( schemeEngine, ic ) ).requestInit();
	}
}
