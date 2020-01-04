package pulsar;

import java.io.IOException;

import kawapad.Kawapad;
import pulsar.lib.scheme.SchemeEngine;
import pulsar.lib.scheme.doc.DescriptiveHelp;
import pulsar.lib.scheme.http.SchemeHttp;
import pulsar.lib.scheme.http.SchemeHttp.UserAuthentication;

public class PulsarApplicationLibrary {
    public static void initializeSchemeEngine( SchemeEngine schemeEngine ) {
        // pulsar gui
        PulsarFrame.registerGlobalSchemeInitializers( schemeEngine );
        Kawapad.registerGlobalSchemeInitializer( schemeEngine );
        // pulsar
        DescriptiveHelp.registerGlobalSchemeInitializer( schemeEngine );
        Pulsar.registerGlobalSchemeInitializers( schemeEngine );
    }

    public static SchemeEngine createSchemeEngine() {
        SchemeEngine schemeEngine = new SchemeEngine();
        initializeSchemeEngine( schemeEngine );
        return schemeEngine;
    }

    public static SchemeHttp createPulsarHttpServer(
            SchemeEngine schemeEngine, 
            int httpPort, UserAuthentication userAuthentication, Pulsar pulsar ) throws IOException {
        return new SchemeHttp( 
            httpPort, 
            userAuthentication, 
            schemeEngine, 
            pulsar.getThreadInitializerCollection());
    }
    public static Pulsar createPulsar( SchemeEngine schemeEngine ) {
        return new Pulsar( schemeEngine );
    }

    public static PulsarFrame createPulsarGui( SchemeEngine schemeEngine, Pulsar pulsar  ) {
        return PulsarFrame.create( pulsar, true , null );
    }


    
}
