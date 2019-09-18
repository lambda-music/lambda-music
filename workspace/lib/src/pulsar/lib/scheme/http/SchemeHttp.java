package pulsar.lib.scheme.http;

import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.lang.invoke.MethodHandles;
import java.net.InetSocketAddress;
import java.nio.charset.Charset;
import java.util.Arrays;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;
import com.sun.net.httpserver.HttpServer;

import pulsar.lib.scheme.SchemeUtils;
import pulsar.lib.scheme.SchemeUtils.ExecuteSchemeResult;
import pulsar.lib.scheme.scretary.SchemeSecretary;


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

public class SchemeHttp {
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


    SchemeSecretary schemeSecretary;
    HttpServer httpServer;
    Charset charset = Charset.forName( "UTF-8" );
    List<Runnable> threadInitializers;
    public SchemeHttp( SchemeSecretary schemeSecretary, int port, List<Runnable> threadInitializers ) throws IOException {
        super();
        this.schemeSecretary = schemeSecretary;
        this.threadInitializers = threadInitializers;
        this.init( port );
    }
    private void init( int port ) throws IOException {
        httpServer = HttpServer.create(new InetSocketAddress( port ), 0);
        httpServer.createContext("/pulsar", new PulsarHttpHandler() );
        httpServer.setExecutor(null); // creates a default executor
        httpServer.start();
        schemeSecretary.addShutdownHook( new Runnable() {
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

    class PulsarHttpHandler implements HttpHandler {
        @Override
        public void handle(HttpExchange t) throws IOException {
            logInfo( "=========PulsarHttp========" );
            logInfo( t.getRemoteAddress().toString() );

            if ( t.getRemoteAddress().getAddress().isLoopbackAddress() ) {
                String requestString = readInputStream( t.getRequestBody() ); 
                logInfo( requestString );
                ExecuteSchemeResult result = SchemeSecretary.evaluateScheme( schemeSecretary, threadInitializers, null, requestString, null, null, "web-scratchpad" );
                String responseString;
                responseString = 
                        SchemeUtils.endWithLineFeed( requestString ) + 
                        SchemeUtils.formatResult( result.valueAsString );
                logInfo( result.valueAsString );
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
