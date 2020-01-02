package pulsar;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import kawapad.Kawapad;
import kawapad.KawapadEvaluator;
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
            SchemeEngine schemeEngine, int httpPort, UserAuthentication userAuthentication, Pulsar pulsar ) throws IOException {
        return new SchemeHttp( 
            httpPort, 
            userAuthentication, 
            schemeEngine, 
            pulsar.getThreadInitializerCollection()
            );
    }
    public static Pulsar createPulsar( SchemeEngine schemeEngine ) {
        Pulsar pulsar = new Pulsar( schemeEngine );
        return pulsar;
    }

    public static PulsarFrame createPulsarGui( SchemeEngine schemeEngine, Pulsar pulsar, List<String> urls ) {
        KawapadEvaluator local = KawapadEvaluator.getLocal();
        ArrayList<KawapadEvaluator> evaluatorList = new ArrayList<>();
        evaluatorList.add( local );
        for ( String url : urls ) {
            evaluatorList.add( KawapadEvaluator.getRemote( url ) );
        }
        PulsarFrame pulsarFrame = PulsarFrame.create( pulsar, local, evaluatorList, true , null );
        return pulsarFrame;
    }


    
}
