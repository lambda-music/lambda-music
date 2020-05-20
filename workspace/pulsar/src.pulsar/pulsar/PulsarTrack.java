package pulsar;

import java.util.List;

import gnu.mapping.Procedure;
import metro.MetroPort;
import metro.MetroSequence;

public class PulsarTrack {
    public static SchemeSequence createSequence( Procedure procedure ){
        return new SchemeSequence( procedure );
    }
    public static MetroSequence createRecordingTrack(  
//        MetroSyncType syncType, 
//        MetroSyncTrack syncTrack, 
//        double syncOffset, 
        List<MetroPort> inputPorts, 
        List<MetroPort> outputPorts,
        double recordLength, 
        boolean looper 
        ) 
    {
        return SchemeRecorderSequence.create( inputPorts, outputPorts, recordLength, looper );
    }

}
