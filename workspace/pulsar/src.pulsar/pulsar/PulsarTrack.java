package pulsar;

import java.util.Collection;
import java.util.List;

import gnu.mapping.Procedure;
import metro.MetroBufferedTrack;
import metro.MetroPort;

public class PulsarTrack {
    public static MetroBufferedTrack createTrack( Object name, Collection<Object> tags, Procedure procedure ) {
        return MetroBufferedTrack.create( name, tags, new SchemeSequence( procedure ) );
    }

    public static MetroBufferedTrack createRecordingTrack( Object name, Collection<Object> tags, List<MetroPort> inputPorts, List<MetroPort> outputPorts,
            double recordLength, boolean looper ) 
    {
        return MetroBufferedTrack.create( name, tags, SchemeSequenceRecorder.create( inputPorts, outputPorts, recordLength, looper ) );
    }

}
