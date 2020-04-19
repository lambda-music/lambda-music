package lamu.lib.scheme;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.io.StringWriter;
import java.net.HttpURLConnection;
import java.net.URL;

import lamu.lib.scheme.socket.SchemeHttp;

public class RemoteEvaluator implements Evaluator, NameCaptionHolder {
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
    public RemoteEvaluator( String url ) {
        this.urlEval  = url + SchemeHttp.PATH_EVAL;
    }
    

    @Override
    public SchemeResult evaluate(
            Runnable threadInitializer, 
            Reader schemeScript, 
            File currentDirectory,
            File currentFile, 
            String currentURI) 
    {
        this.setCurrentEvaluator();

        String schemeScriptString;
        try {
            schemeScriptString = readAllSchemeScript( schemeScript );
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
    
    private static String readAllSchemeScript( Reader schemeScript ) throws IOException {
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
    @Override
    public String toString() {
        return "[http " +  this.urlEval + "]";
    }
    @Override
    public String getNameCaption() {
        return "http " +  this.urlEval + "";
    }
    @Override
    public void initializeEvaluator() {
    }
    @Override
    public void finalizeEvaluator() {
    }
    
}
