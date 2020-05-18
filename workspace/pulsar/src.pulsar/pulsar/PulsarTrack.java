package pulsar;

import java.util.Collection;
import java.util.List;

import gnu.mapping.Procedure;
import metro.MetroPort;
import metro.MetroSyncType;
import metro.MetroTrack;
import metro.MetroTradTrackSynchronizer;

public class PulsarTrack {
    public static SchemeSequence createTrack( Object name, Collection<Object> tags, 
        MetroSyncType syncType, MetroTrack syncTrack, double syncOffset, Procedure procedure ) 
    {
        return new SchemeSequence( name, tags, MetroTradTrackSynchronizer.create( syncType, syncTrack, syncOffset), procedure );
    }

    public static SchemeSequenceRecorder createRecordingTrack( Object name, Collection<Object> tags, 
//        MetroSyncType syncType, 
//        MetroSyncTrack syncTrack, 
//        double syncOffset, 
        List<MetroPort> inputPorts, 
        List<MetroPort> outputPorts,
        double recordLength, 
        boolean looper 
        ) 
    {
        return SchemeSequenceRecorder.create( name, tags, inputPorts, outputPorts, recordLength, looper );
    }

}
