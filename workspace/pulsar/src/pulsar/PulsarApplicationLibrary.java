package pulsar;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import kawapad.Kawapad;
import kawapad.KawapadEvaluator;
import pulsar.lib.scheme.doc.DescriptiveHelp;
import pulsar.lib.scheme.http.SchemeHttp;
import pulsar.lib.scheme.http.SchemeHttp.UserAuthentication;
import pulsar.lib.scheme.scretary.SchemeExecutor;

public class PulsarApplicationLibrary {
    public static void initializeSchemeExecutor( SchemeExecutor schemeExecutor ) {
        // pulsar gui
        PulsarFrame.registerGlobalSchemeInitializers( schemeExecutor );
        Kawapad.registerGlobalSchemeInitializer( schemeExecutor );
        // pulsar
        DescriptiveHelp.registerGlobalSchemeInitializer( schemeExecutor );
        Pulsar.registerGlobalSchemeInitializers( schemeExecutor );
    }

    public static SchemeExecutor createSchemeExecutor() {
        SchemeExecutor schemeExecutor = new SchemeExecutor();
        initializeSchemeExecutor( schemeExecutor );
        return schemeExecutor;
    }

    public static SchemeHttp createPulsarHttpServer(
            SchemeExecutor schemeExecutor, int httpPort, UserAuthentication userAuthentication, Pulsar pulsar ) throws IOException {
        return new SchemeHttp( 
            httpPort, 
            userAuthentication, 
            schemeExecutor, 
            pulsar.getThreadInitializerCollection()
            );
    }
    public static Pulsar createPulsar( SchemeExecutor schemeExecutor ) {
        Pulsar pulsar = new Pulsar( schemeExecutor );
        return pulsar;
    }

    public static PulsarFrame createPulsarGui( SchemeExecutor schemeExecutor, Pulsar pulsar, List<String> urls ) {
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
