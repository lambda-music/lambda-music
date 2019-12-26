package kawapad;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;

import pulsar.lib.scheme.SchemeResult;

final class KawapadEvaluatorRemote implements KawapadEvaluator {
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
    private String url;
    public KawapadEvaluatorRemote(String url) {
        this.url = url;
    }
    @Override
    public String getName() {
        return this.url;
    }
    @Override
    public SchemeResult evaluate(Kawapad kawapad, String schemeScript)  {
        try {
            String result = httpRequest( url, schemeScript );
            return SchemeResult.createSucceeded( false, SchemeResult.UNKNOWN_CONTENT, result );
        } catch (IOException e) {
            return SchemeResult.createError( e );
        }
    }
}