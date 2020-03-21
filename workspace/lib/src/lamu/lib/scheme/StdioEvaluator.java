package lamu.lib.scheme;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.io.StringWriter;
import java.lang.invoke.MethodHandles;
import java.util.concurrent.CountDownLatch;
import java.util.logging.Level;

import gnu.mapping.SimpleSymbol;
import gnu.mapping.Symbol;
import lamu.lib.log.Logger;
import lamu.lib.scheme.repl.ReplClient;
import lamu.lib.scheme.repl.ReplClient.ReplClientResultReceiver;
import lamu.lib.stream.SisoReceiver;
import lamu.lib.stream.Streamable;

public class StdioEvaluator implements ServicingEvaluator {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }

    private Streamable stream;
    private InputStream in;
    private OutputStream out;
    private InputStream err;
    private SisoReceiver sisoReceiver;
    private ReplClient replClient;
    public StdioEvaluator( Streamable stream ) {
        this.stream = stream;
    }

    final class ReplClientResultReceiverImplementation implements ReplClientResultReceiver {
        CountDownLatch latch;
        String scriptResult;
        public ReplClientResultReceiverImplementation( CountDownLatch latch ) {
            this.latch = latch;
        }
        @Override
        public void receive( String scriptResult ) {
            this.scriptResult = scriptResult;
        }
    }
    static final SimpleSymbol SUCCEEDED = Symbol.valueOf("succeeded");
    static final SimpleSymbol FAILED = Symbol.valueOf("failed");

    @Override
    public SchemeResult evaluate( 
        Runnable threadInitializer, Reader schemeScript, File currentDirectory, File currentFile, String currentURI ) 
    {
        String scriptString;
        try {
             scriptString = readAllSchemeScript( schemeScript );
        } catch (IOException e) {
            throw new RuntimeException( e );
        }
        CountDownLatch latch = new CountDownLatch(1);
        ReplClientResultReceiverImplementation resultReceiver = new ReplClientResultReceiverImplementation( latch );
        replClient.exec( scriptString, resultReceiver );
        try {
            latch.await();
        } catch (InterruptedException e) {
            throw new RuntimeException( e );
        }
        
        Object lispValue;
        try {
            lispValue = SchemeUtils.string2scheme( resultReceiver.scriptResult );
        } catch (IOException e) {
            throw new RuntimeException( e );
        }
        Object lispValueStatus = SchemeUtils.car( SchemeUtils.assq( "status" ,  lispValue ) );
        Object lispValueMessage= SchemeUtils.car( SchemeUtils.assq( "message" , lispValue ) );
        if ( SUCCEEDED.equals( lispValueStatus ) ) {
            return SchemeResult.createSucceededByObject( lispValueMessage );
            
        } else if ( FAILED.equals( lispValueStatus ) ) {
            return SchemeResult.createError( SchemeUtils.schemeStringToJavaString( lispValueMessage ) );
        } else {
            throw new RuntimeException( "" );
        }
    }

    @Override
    public void reset() {
        replClient.reset();
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
    public void initializeEvaluator() {
        logInfo("StdioEvaluator:initializeEvaluator");
        this.err = stream.getErrorStream();
        this.in = stream.getInputStream();
        this.out = stream.getOutputStream();
        
        this.replClient = new ReplClient();
        this.sisoReceiver = new SisoReceiver( null, this.in, this.out, this.replClient );
        this.sisoReceiver.requestInit();

        
    }
    @Override
    public void finalizeEvaluator() {
        this.sisoReceiver.requestQuit();
        try {
            this.in.close();
        } catch (IOException e) {
            logError("warning",e);
        }
        try {
            this.out.close();
        } catch (IOException e) {
            logError("warning",e);
        }
        try {
            this.err.close();
        } catch (IOException e) {
            logError("warning",e);
        }
    }
}
