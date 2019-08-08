package pulsar;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.lang.invoke.MethodHandles;
import java.net.InetSocketAddress;
import java.nio.charset.Charset;
import java.util.Arrays;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;
import com.sun.net.httpserver.HttpServer;

import gnu.kawa.io.InPort;
import kawa.standard.Scheme;
import pulsar.lib.scheme.SimpleSchemePrettifier;
import pulsar.lib.scheme.scretary.SchemeSecretary;
import pulsar.lib.secretary.Invokable;
import pulsar.lib.secretary.SecretaryMessage;


/**
 * This class enables you to execute scripts from your editor. For example, in
 * vim you can achieve it by creating a new keybind as following :
 * 
 * > xmap <Return> :!curl -sSd "`cat`" http://localhost:8192/pulsar^M^M
 * 
 * After creating the keybind, press enter after entering visual-mode in vim to
 * execute the selection as scheme program in the current Pulsar application
 * instance.
 * 
 * This class uses {@link com.sun.net.httpserver.HttpServer} class. Usually
 * these com.sun.* classes should not be used by non-system applications. But I
 * believe that it is allowed to use these class because it is marked
 * as @jdk.Exported. 
 * 
 * @see <a href="https://www.google.com/search?q=jdk.Exported">https://www.google.com/search?q=jdk.Exported</a> 
 * 
 */

class PulsarHttp {
	static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
	static void logError(String msg, Throwable e) {
		LOGGER.log(Level.SEVERE, msg, e);
	}
	static void logInfo(String msg) {
		LOGGER.log(Level.INFO, msg);
	}
	static void logWarn(String msg) {
		LOGGER.log(Level.WARNING, msg);
	}

	
	Pulsar pulsar;
	HttpServer httpServer;
	Charset charset = Charset.forName( "UTF-8" );
	public PulsarHttp( Pulsar pulsar, int port ) throws IOException {
		super();
		this.pulsar = pulsar;
		this.init( port );
	}
	private void init( int port ) throws IOException {
		httpServer = HttpServer.create(new InetSocketAddress( port ), 0);
        httpServer.createContext("/pulsar", new PulsarHttpHandler() );
        httpServer.setExecutor(null); // creates a default executor
        httpServer.start();
        pulsar.addShutdownHook( new Runnable() {
			@Override
			public void run() {
				stop();
			}
		});
	}
	
	void start(){
		this.httpServer.start();
	}
	void stop(){
		this.httpServer.stop(0);
	}
	
	static String readInputStream( InputStream inputStream ) throws IOException {
		BufferedInputStream bis = new BufferedInputStream(inputStream);
		ByteArrayOutputStream buf = new ByteArrayOutputStream();
		int result = bis.read();
		while(result != -1) {
			buf.write((byte) result);
			result = bis.read();
		}
		// StandardCharsets.UTF_8.name() > JDK 7
		try {
			return buf.toString("UTF-8");
		} catch (UnsupportedEncodingException e) {
			e.printStackTrace();
			return buf.toString();
		}
	}
	
	public static String executeScheme( SchemeSecretary schemeSecretary, Charset charset, String schemeScript, boolean requestResult ) throws IOException {
		schemeSecretary.initializeSchemeForCurrentThread();
		
		ByteArrayOutputStream bo = new ByteArrayOutputStream();
		ByteArrayInputStream bi = new ByteArrayInputStream( schemeScript.getBytes( charset ));
		try {
			Object result = schemeSecretary.executeWithoutSecretarially( new SecretaryMessage<Scheme,Object,Throwable>(){
				@Override
				public Object execute(Scheme resource, Object[] args) throws Throwable {
					return resource.eval( new InPort( bi ) );
				}
			}, Invokable.NOARG );
			if ( requestResult ) {
				if ( result == null ) {
					bo.write( "#!null".getBytes() );
				} else {
					bo.write( SimpleSchemePrettifier.prettyPrint( result ).getBytes( charset ) );
					bo.write( '\n' );
				}
			} else {
			}
		} catch (Throwable e) {
			PrintWriter w = new PrintWriter( bo );
			e.printStackTrace( w );
			w.flush();
		}
		bo.flush();

		return new String( bo.toByteArray(), charset );
	}

	class PulsarHttpHandler implements HttpHandler {
		boolean requestResult = true; 
		@Override
		public void handle(HttpExchange t) throws IOException {
			logInfo( "=========PulsarHttp========" );
			logInfo( t.getRemoteAddress().toString() );
			
			if ( t.getRemoteAddress().getAddress().isLoopbackAddress() ) {
				String requestString = readInputStream( t.getRequestBody() ); 
				logInfo( requestString );

				String result = executeScheme( pulsar.getSchemeSecretary(), charset, requestString, requestResult );

				String responseString;
				if ( ! result.equals( "" ) ) {
					if ( result.endsWith("\n") )
						responseString = requestString + "\n#|\n" + result   + "|#\n";
					else
						responseString = requestString + "\n#|\n" + result + "\n|#\n";
				} else {
					responseString = requestString;
				}

				t.sendResponseHeaders(200, responseString.length());
				t.getResponseHeaders().put( "Content-Type",  Arrays.asList( "text/plain; charset=utf-8" ) );
				OutputStream os = t.getResponseBody();
				os.write(responseString.getBytes());
				os.close();
			} else {
				// forbidden
				String response = "";
				t.sendResponseHeaders(403, response.length());
				t.getResponseHeaders().put( "Content-Type",  Arrays.asList( "text/plain; charset=utf-8" ) );
				OutputStream os = t.getResponseBody();
				os.write(response.getBytes());
				os.close();
			}
		}
	}


}