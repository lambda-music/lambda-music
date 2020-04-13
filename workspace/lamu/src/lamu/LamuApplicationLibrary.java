package lamu;

import java.io.IOException;

import kawapad.KawapadFrame;
import lamu.lib.scheme.SchemeEngine;
import lamu.lib.scheme.doc.old.DescriptiveHelp;
import lamu.lib.scheme.socket.SchemeHttp;
import lamu.lib.scheme.socket.SchemeHttp.UserAuthentication;
import pulsar.Pulsar;
import pulsar.PulsarFrame;

public class LamuApplicationLibrary {
    public static void initializeSchemeEngine( SchemeEngine schemeEngine ) {
        // pulsar
        DescriptiveHelp.registerGlobalSchemeInitializer( schemeEngine );
        Pulsar.registerSchemeInitializers( schemeEngine );
    }

    public static SchemeEngine createSchemeEngine() {
        SchemeEngine schemeEngine = new SchemeEngine();
        initializeSchemeEngine( schemeEngine );
        return schemeEngine;
    }

    public static SchemeHttp createPulsarHttpServer(
            SchemeEngine schemeEngine, 
            int httpPort, 
            String path, 
            UserAuthentication userAuthentication ) throws IOException 
    {
        return new SchemeHttp( 
            httpPort,
            path,
            userAuthentication, 
            schemeEngine );
    }
    public static Pulsar createPulsar() {
        return new Pulsar();
    }

    public static PulsarFrame createPulsarGui( SchemeEngine schemeEngine  ) {
        return PulsarFrame.create( schemeEngine, true , null );
    }

    public static KawapadFrame createKawapadGui( SchemeEngine schemeEngine ) {
        return new KawapadFrame( schemeEngine, true, "Scheme Scratch Pad" );
    }


    
}
