package quartz.lib.scheme;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.io.StringWriter;
import java.net.HttpURLConnection;
import java.net.URL;

import quartz.lib.scheme.socket.SchemeHttp;

public class RemoteEvaluator implements Evaluator {
    public static String httpRequest(String urlString, String postString) throws IOException {
        String outputString = postString; 
        URL url = new URL( urlString );
        HttpURLConnection c = (HttpURLConnection) url.openConnection();
        c.setRequestMethod( "POST" );
        c.setRequestProperty("Content-Type", "text/plain; utf-8");
        c.setRequestProperty("Accept", "text/plain");
        c.setDoOutput(true);
        c.connect();
        try( OutputStream os = c.getOutputStream() ) {
            byte[] o = outputString.getBytes("utf-8");
            os.write(o, 0, o.length);           
        }
        try( InputStream i = c.getInputStream()) {
            ByteArrayOutputStream bout = new ByteArrayOutputStream();
            byte[] buf = new byte[ 1024 * 16 ];
            while ( true ) {
                int len = i.read( buf );
                if ( len < 0 )
                    break;
                bout.write( buf, 0, len );
            }
            return new String( bout.toByteArray(), "utf-8" );
        }
    }
    private String urlEval;
    private String urlReset;
    public RemoteEvaluator( String url ) {
        this.urlEval  = url + SchemeHttp.PATH_EVAL;
        this.urlReset = url + SchemeHttp.PATH_RESET;
    }
    @Override
    public String toString() {
        return this.urlEval;
    }

    @Override
    public SchemeResult evaluate(
            Runnable threadInitializer, 
            Reader schemeScript, 
            File currentDirectory,
            File currentFile, 
            String currentURI) 
    {
        String schemeScriptString;
        try {
            schemeScriptString = readAll( schemeScript );
        } catch (IOException e1) {
            throw new RuntimeException(e1);
        }
        
        try {
            String result = httpRequest( urlEval, schemeScriptString );
            return SchemeResult.createSucceededByString( result );
        } catch (IOException e) {
            return SchemeResult.createError( e );
        }
    }
    
    @Override
    public void reset() {
        try {
            String result = httpRequest( urlReset, "reset\n" );
            Evaluator.logInfo( "RemoteEvaluator.reset() :" +  result );
        } catch (IOException e) {
            Evaluator.logError( "RemoteEvaluator.reset() error :" , e  );
        }
    }
    
    private static String readAll(Reader schemeScript) throws IOException {
        try ( StringWriter w = new StringWriter();    
              Reader r = schemeScript ) 
        {
            char[] cbuf = new char[ 1024 * 8 ];
            for (;;) {
                int size = r.read( cbuf );
                if ( 0 < size ) {
                    w.write( cbuf , 0, size );
                } else {
                    break;
                }
            }
            return w.getBuffer().toString();
        }
    }
}
