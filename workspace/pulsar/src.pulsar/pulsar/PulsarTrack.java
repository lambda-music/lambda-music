package pulsar;

import java.util.Collection;
import java.util.List;

import gnu.mapping.Procedure;
import metro.MetroPort;
import metro.MetroTrack;
import metro.MetroSeqSynchronizer;

public class PulsarTrack {
    public static MetroTrack createTrack( Object name, Collection<Object> tags, MetroSeqSynchronizer trackSynchronizer, Procedure procedure ){
        return new MetroTrack( name, tags, new SchemeSequence( trackSynchronizer, procedure ));
    }

    public static MetroTrack createRecordingTrack( Object name, Collection<Object> tags, 
//        MetroSyncType syncType, 
//        MetroSyncTrack syncTrack, 
//        double syncOffset, 
        List<MetroPort> inputPorts, 
        List<MetroPort> outputPorts,
        double recordLength, 
        boolean looper 
        ) 
    {
        return new MetroTrack( name, tags, SchemeRecorderTrackSeq.create( inputPorts, outputPorts, recordLength, looper ));
    }

}
