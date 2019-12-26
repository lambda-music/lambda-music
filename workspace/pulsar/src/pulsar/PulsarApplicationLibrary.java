package pulsar;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import kawapad.Kawapad;
import kawapad.KawapadEvaluator1;
import pulsar.lib.scheme.doc.DescriptiveHelp;
import pulsar.lib.scheme.http.SchemeHttp;
import pulsar.lib.scheme.http.SchemeHttp.UserAuthentication;
import pulsar.lib.scheme.scretary.SchemeSecretary;

public class PulsarApplicationLibrary {
    public static void initializeSchemeSecretary( SchemeSecretary schemeSecretary ) {
        // pulsar gui
        PulsarFrame.registerGlobalSchemeInitializers( schemeSecretary );
        Kawapad.registerGlobalSchemeInitializer( schemeSecretary );
        // pulsar
        DescriptiveHelp.registerGlobalSchemeInitializer( schemeSecretary );
        Pulsar.registerGlobalSchemeInitializers( schemeSecretary );
    }

    public static SchemeSecretary createSchemeSecretary() {
        SchemeSecretary schemeSecretary = new SchemeSecretary();
        schemeSecretary.setDirectMeeting( true );
        initializeSchemeSecretary( schemeSecretary );
        return schemeSecretary;
    }

    public static SchemeHttp createPulsarHttpServer(
            SchemeSecretary schemeSecretary, int httpPort, UserAuthentication userAuthentication, Pulsar pulsar ) throws IOException {
        return new SchemeHttp( 
            httpPort, 
            userAuthentication, 
            schemeSecretary, 
            pulsar.getThreadInitializerCollection()
            );
    }
    public static Pulsar createPulsar( SchemeSecretary schemeSecretary ) {
        Pulsar pulsar = new Pulsar( schemeSecretary );
        return pulsar;
    }

    public static PulsarFrame createPulsarGui( SchemeSecretary schemeSecretary, Pulsar pulsar, List<String> urls ) {
        KawapadEvaluator1 local = KawapadEvaluator1.getLocal();
        ArrayList<KawapadEvaluator1> evaluatorList = new ArrayList<>();
        evaluatorList.add( local );
        for ( String url : urls ) {
            evaluatorList.add( KawapadEvaluator1.getRemote( url ) );
        }
        PulsarFrame pulsarFrame = PulsarFrame.create( pulsar, local, evaluatorList, true , null );
        return pulsarFrame;
    }


    
}
