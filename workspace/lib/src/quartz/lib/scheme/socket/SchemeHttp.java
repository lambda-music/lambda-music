package quartz.lib.scheme.socket;

import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.lang.invoke.MethodHandles;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.URLDecoder;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;
import com.sun.net.httpserver.HttpServer;

import quartz.lib.CurrentObject;
import quartz.lib.app.ApplicationComponent;
import quartz.lib.log.LamuLogger;
import quartz.lib.scheme.SchemeEngine;
import quartz.lib.scheme.SchemeResult;
import quartz.lib.thread.ThreadInitializer;
import quartz.lib.thread.ThreadInitializerCollection;
import quartz.lib.thread.ThreadInitializerCollectionContainer;
import quartz.lib.thread.ThreadInitializerContainer;


/**
 * This class uses {@link com.sun.net.httpserver.HttpServer} class. Usually
 * these com.sun.* classes should not be used by non-system applications. But I
 * believe that it is allowed to use these class because it is marked
 * as @jdk.Exported.
 * 
 * @see <a href="https://www.google.com/search?q=jdk.Exported">https://www.google.com/search?q=jdk.Exported</a> 
 * 
 */

public class SchemeHttp implements ThreadInitializerContainer<SchemeHttp>, ThreadInitializerCollectionContainer, ApplicationComponent {
    public static final String PATH_VIM = "/vim";
    public static final String PATH_RESET = "/reset";
    public static final String PATH_EVAL  = "/eval";
    static final Logger LOGGER = LamuLogger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) {
        LOGGER.log(Level.SEVERE, msg, e);
    }
    static void logInfo(String msg) {
        LOGGER.log(Level.INFO, msg);
    }
    static void logWarn(String msg) {
        LOGGER.log(Level.WARNING, msg);
    }

    ////////////////////////////////////////////////////////////////////////////
    //
    ////////////////////////////////////////////////////////////////////////////

    private ApplicationComponent parentApplicationComponent;
    @Override
    public ApplicationComponent getParentApplicationComponent() {
        return this.parentApplicationComponent;
    }
    @Override
    public void setParentApplicationComponent(ApplicationComponent parentApplicationComponent) {
        this.parentApplicationComponent = parentApplicationComponent;
    }

    @Override
    public void processInit() {
        
    }
    @Override
    public void processQuit() {
        stop();
    }

    ////////////////////////////////////////////////////////////////////////////
    // The Thread Initializer Facility
    ////////////////////////////////////////////////////////////////////////////

    private static final CurrentObject<SchemeHttp> currentObject = new CurrentObject<>( SchemeHttp.class );
    private final ThreadInitializer<SchemeHttp> threadInitializer = 
            ThreadInitializer.createMultipleThreadInitializer( "scheme-http", this, 
                ThreadInitializer.createThreadInitializer( "scheme-http-current", currentObject, this ) );
    @Override
    public ThreadInitializer<SchemeHttp> getThreadInitializer() {
        return threadInitializer;
    }    
    public static SchemeHttp getCurrent() {
        return currentObject.get();
    }
    public static boolean isPresent() {
        return currentObject.isPresent();
    }

    ////////////////////////////////////////////////////////////////////////////

    private ThreadInitializerCollection threadInitializerCollection = new ThreadInitializerCollection( "scheme", this );
    {
        threadInitializerCollection.addThreadInitializer( this.getThreadInitializer() );
    }
    public ThreadInitializerCollection getThreadInitializerCollection() {
        return threadInitializerCollection;
    }

    ////////////////////////////////////////////////////////////////////////////
    // 
    ////////////////////////////////////////////////////////////////////////////

    private static String decode(String value) {
        try {
            return URLDecoder.decode(value, StandardCharsets.UTF_8.toString());
        } catch (UnsupportedEncodingException e) {
            throw new InternalError( e );
        }
    }
    public static final Map<String, String> decodeQuery(String query) {
        HashMap<String,String> map = new HashMap<>();
        String[] elems = query.split("&");
        for ( String e : elems ) {
            String[] keyValue = e.split( "=" );
            map.put( decode( keyValue[0] ), decode( keyValue[1] ) );  
        }
        return map;
    }

    public interface UserAuthentication {
        UserAuthentication ONLY_LOOPBACK = new UserAuthentication() {
            @Override
            public boolean check(HttpExchange httpExchange) {
                InetAddress remoteAddress = httpExchange.getRemoteAddress().getAddress();
                return remoteAddress.isLoopbackAddress();
            }
        };
        public static UserAuthentication createAddressRestricted( String userToken, InetAddress ... userAddresses  ) {
            return new UserAuthentication() {
                List<InetAddress> addresses = Arrays.asList( userAddresses );
                @Override
                public boolean check( HttpExchange httpExchange) {
                    InetAddress remoteAddress = httpExchange.getRemoteAddress().getAddress();
                    Map<String, String> query = decodeQuery( httpExchange.getRequestURI().getQuery() );

                    boolean found = false;
                    for ( InetAddress a : addresses ) { 
                        if ( remoteAddress.equals( a ) ) {
                            found = true;
                            break;
                        }
                    }
                    String token = query.get( "token"  );
                    
                    return ( found && token != null && token.equals( userToken ) );
                }
            };
        }
        boolean check( HttpExchange httpExchange );
    }


    int port;
    SchemeEngine schemeEngine;
    HttpServer httpServer;
    Charset charset = Charset.forName( "UTF-8" );
    UserAuthentication authentication;
    public SchemeHttp( int port, String path, UserAuthentication authentication, SchemeEngine schemeEngine ) throws IOException {
        super();
        this.port = port;
        this.authentication = authentication;
        this.schemeEngine = schemeEngine;
        this.initialize( path );
    }
    private void initialize( String path ) throws IOException {
        httpServer = HttpServer.create(new InetSocketAddress( port ), 0);
        httpServer.createContext( path + PATH_VIM,     new VimSchemeEvaluation(  this.authentication) );
        httpServer.createContext( path + PATH_EVAL,  new PlainSchemeEvaluation(this.authentication) );
        httpServer.createContext( path + PATH_RESET, new ResetScheme(          this.authentication) );
        
        httpServer.setExecutor(null); // creates a default executor
        httpServer.start();
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
    
    static void logHandle(HttpExchange t, Class<?> c ) {
        logInfo( "=========" + c.getSimpleName() + "========" );
        logInfo( t.getRemoteAddress().toString() );
    }

    abstract class SchemeHttpHandler implements HttpHandler {
        final UserAuthentication authentication;
        public SchemeHttpHandler(UserAuthentication authentication) {
            super();
            this.authentication = authentication;
        }

        public abstract void handleProc(HttpExchange exchange) throws IOException;
        
        @Override
        public final void handle(HttpExchange t) throws IOException {
            logHandle( t, this.getClass() );
            if ( authentication.check( t ) ) {
                handleProc( t );
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

    /*
     * This class enables you to execute scripts from your editor. For example, in
     * vim you can achieve it by creating a new keybind as following :
     * 
     * > xmap <Return> :!curl -sSd "`cat`" http://localhost:8192/pulsar^M^M
     * 
     * After creating the keybind, press enter after entering visual-mode in vim to
     * execute the selection as scheme program in the current Pulsar application
     * instance.
     * 
     */
    class VimSchemeEvaluation extends SchemeHttpHandler {
        public VimSchemeEvaluation(UserAuthentication authentication) {
            super( authentication );
        }

        @Override
        public void handleProc(HttpExchange t) throws IOException {
            String requestString = readInputStream( t.getRequestBody() ); 
            logInfo( requestString );
            SchemeResult schemeResult = 
                    schemeEngine.getSchemeEvaluator().evaluate( 
                        SchemeHttp.this.getThreadInitializerCollection(),    
                        requestString, 
                        null, 
                        null, 
                        "web-scratchpad" );
            String responseString;
            
            if ( schemeResult.getValue() == null ) {
                responseString = 
                        SchemeResult.endWithLineFeed( requestString ); 
            } else {
                responseString = 
                        SchemeResult.endWithLineFeed( requestString ) + 
                        SchemeResult.formatResult( schemeResult.getValueAsString() );
            }
            logInfo( schemeResult.getValueAsString() );
            t.sendResponseHeaders(200, responseString.length());
            t.getResponseHeaders().put( "Content-Type",  Arrays.asList( "text/plain; charset=utf-8" ) );
            OutputStream os = t.getResponseBody();
            os.write(responseString.getBytes());
            os.close();
        }
    }

    class PlainSchemeEvaluation extends SchemeHttpHandler {
        public PlainSchemeEvaluation(UserAuthentication authentication) {
            super( authentication );
        }

        @Override
        public void handleProc(HttpExchange t) throws IOException {
            String requestString = readInputStream( t.getRequestBody() ); 
            logInfo( requestString );
            SchemeResult schemeResult = schemeEngine.getSchemeEvaluator().evaluate( 
                                            SchemeHttp.this.getThreadInitializerCollection() ,    
                                            requestString, 
                                            null, 
                                            null, 
                                            "web-scratchpad" );
            String responseString;
            responseString = schemeResult.getValueAsString();
            logInfo( schemeResult.getValueAsString() );
            t.sendResponseHeaders(200, responseString.length());
            t.getResponseHeaders().put( "Content-Type",  Arrays.asList( "text/plain; charset=utf-8" ) );
            OutputStream os = t.getResponseBody();
            os.write(responseString.getBytes());
            os.close();
        }
    }

    class ResetScheme extends SchemeHttpHandler {
        public ResetScheme(UserAuthentication authentication) {
            super( authentication );
        }

        @Override
        public void handleProc(HttpExchange t) throws IOException {
            String requestString = readInputStream( t.getRequestBody() );
            logInfo( requestString );
            
            schemeEngine.getEvaluatorManager().getCurrentEvaluator().reset();
            
            String responseString;
            responseString = "ok";
            logInfo( responseString );
            t.sendResponseHeaders(200, responseString.length());
            t.getResponseHeaders().put( "Content-Type",  Arrays.asList( "text/plain; charset=utf-8" ) );
            OutputStream os = t.getResponseBody();
            os.write(responseString.getBytes());
            os.close();
        }
    }

    public void init() {
    }
}
