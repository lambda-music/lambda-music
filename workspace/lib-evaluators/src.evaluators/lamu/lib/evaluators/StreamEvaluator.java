package lamu.lib.evaluators;

import java.io.File;
import java.io.IOException;
import java.io.Reader;
import java.io.StringWriter;
import java.lang.invoke.MethodHandles;
import java.util.concurrent.CountDownLatch;
import java.util.logging.Level;

import gnu.mapping.SimpleSymbol;
import gnu.mapping.Symbol;
import lamu.lib.evaluators.repl.ReplClient;
import lamu.lib.evaluators.repl.ReplClient.ReplClientResultReceiver;
import lamu.lib.kawautils.SchemeValues;
import lamu.lib.logging.Logger;
import lamu.lib.streams.NullOutputStream;
import lamu.lib.streams.SisoReceiver;
import lamu.lib.streams.Stream;
import lamu.lib.streams.StreamPump;

public class StreamEvaluator implements Evaluator, NameCaptionHolder {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }

    private Stream stream;
    private SisoReceiver sisoReceiver;
    private ReplClient replClient;
    public StreamEvaluator( Stream stream ) {
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
            this.latch.countDown();
        }
    }
    static final SimpleSymbol SUCCEEDED = Symbol.valueOf("succeeded");
    static final SimpleSymbol FAILED = Symbol.valueOf("failed");
    private StreamPump errorPump;

    @Override
    public SchemeResult evaluate( 
        Runnable threadInitializer, Reader schemeScript, File currentFile, String currentURI ) 
    {
        this.setCurrentEvaluator();

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
            lispValue = SchemeValues.string2lisp( resultReceiver.scriptResult );
        } catch (IOException e) {
            throw new RuntimeException( e );
        }
        Object lispValueStatus = SchemeValues.cdr( SchemeValues.assq( "status" ,  lispValue ) );
        Object lispValueMessage= SchemeValues.cdr( SchemeValues.assq( "message" , lispValue ) );
        if ( SUCCEEDED.equals( lispValueStatus ) ) {
            return SchemeResult.createSucceededByObject( lispValueMessage );
            
        } else if ( FAILED.equals( lispValueStatus ) ) {
            return SchemeResult.createError( SchemeValues.toString( lispValueMessage ) );
        } else {
            throw new RuntimeException( "" );
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
    public void initializeEvaluator() {
        logInfo("StdioEvaluator:initializeEvaluator");
        
        this.replClient = new ReplClient();
        this.sisoReceiver = new SisoReceiver( this.stream, this.replClient );
        this.sisoReceiver.requestInit();

        this.errorPump = new StreamPump( this.stream.getDownwardErrorStream(), NullOutputStream.INSTANCE );
        this.errorPump.requestInit();
    }
    @Override
    public void finalizeEvaluator() {
        this.sisoReceiver.requestQuit();
        this.errorPump.requestQuit();
        try {
            this.stream.getDownwardStream().close();
        } catch (IOException e) {
            logError("warning",e);
        }
        try {
            this.stream.getUpwardStream().close();
        } catch (IOException e) {
            logError("warning",e);
        }
        try {
            this.stream.getDownwardErrorStream().close();
        } catch (IOException e) {
            logError("warning",e);
        }
    }

    @Override
    public String toString() {
        return "[" + stream.toString() + "]";
    }
    @Override
    public String getNameCaption() {
        return "" + stream.toString() + "";
    }
}
