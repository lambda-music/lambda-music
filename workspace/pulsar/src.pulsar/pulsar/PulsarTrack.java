package pulsar;

import java.util.Collection;
import java.util.List;

import gnu.mapping.Procedure;
import metro.MetroPort;

public class PulsarTrack {
    public static SchemeSequence createTrack( Object name, Collection<Object> tags, Procedure procedure ) {
        return new SchemeSequence( name, tags, procedure );
    }

    public static SchemeSequenceRecorder createRecordingTrack( Object name, Collection<Object> tags, List<MetroPort> inputPorts, List<MetroPort> outputPorts,
            double recordLength, boolean looper ) 
    {
        return SchemeSequenceRecorder.create( name, tags, inputPorts, outputPorts, recordLength, looper );
    }

}
