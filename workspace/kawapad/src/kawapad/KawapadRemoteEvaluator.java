package kawapad;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;

import pulsar.lib.scheme.SchemeResult;

public final class KawapadRemoteEvaluator implements KawapadEvaluator, KawapadName {
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
    
    String url;
    public KawapadRemoteEvaluator(String url) {
        super();
        this.url = url;
    }
    
    @Override
    public String getName() {
        return this.url;
    }

    @Override
    public void evaluate( Kawapad kawapad, String text, boolean doInsertText, boolean doReplaceText, boolean doReset ) {
        kawapad.getThreadManager().startScratchPadThread( 
            new KawapadRemoveEvaluatorRunnable( kawapad, text, doInsertText, doReplaceText, doReset ) );
    }
    
    class KawapadRemoveEvaluatorRunnable extends KawapadEvaluatorRunnable {
        public KawapadRemoveEvaluatorRunnable(
                Kawapad kawapad, String schemeScript, 
                boolean insertText, boolean replaceText, boolean doReset ) 
        { 
            super( kawapad, schemeScript, insertText, replaceText, true, doReset );
        }
        
        @Override
        public SchemeResult evaluate()  {
            try {
                String result = httpRequest( url, schemeScript );
                return SchemeResult.createSucceeded( false, SchemeResult.UNKNOWN_CONTENT, result );
            } catch (IOException e) {
                return SchemeResult.createError( e );
            }
        }
    }
}