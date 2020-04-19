package lamu;

import java.io.IOException;

import kawapad.KawapadFrame;
import lamu.lib.scheme.MultiplexEvaluator;
import lamu.lib.scheme.socket.SchemeHttp;
import lamu.lib.scheme.socket.SchemeHttp.UserAuthentication;
import pulsar.Pulsar;
import pulsar.PulsarFrame;

public class LamuApplicationLibrary {
    public static SchemeHttp createPulsarHttpServer(
            MultiplexEvaluator multiplexEvaluator, 
            int httpPort, 
            String path, 
            UserAuthentication userAuthentication ) throws IOException 
    {
        return new SchemeHttp( 
            httpPort,
            path,
            userAuthentication, 
            multiplexEvaluator );
    }
    public static Pulsar createPulsar() {
        return new Pulsar();
    }

    public static PulsarFrame createPulsarGui( MultiplexEvaluator multiplexEvaluator  ) {
        return PulsarFrame.create( multiplexEvaluator, true , null );
    }

    public static KawapadFrame createKawapadGui( MultiplexEvaluator multiplexEvaluator ) {
        return new KawapadFrame( multiplexEvaluator, true, "Scheme Scratch Pad" );
    }


    
}
