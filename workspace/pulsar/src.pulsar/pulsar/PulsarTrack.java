package pulsar;

import java.util.Collection;
import java.util.List;

import gnu.mapping.Procedure;
import metro.MetroPort;
import metro.MetroTrack;

public class PulsarTrack {

    public static MetroTrack createTrack( Object name, Collection<Object> tags, Procedure procedure ) {
        return MetroTrack.create( name, tags, new SchemeSequence( procedure ) );
    }

    public static MetroTrack createRecordingTrack( Object name, Collection<Object> tags, List<MetroPort> inputPorts, List<MetroPort> outputPorts,
            double recordLength, boolean looper ) 
    {
        return MetroTrack.create( name, tags, SchemeSequenceRecorder.create( inputPorts, outputPorts, recordLength, looper ) );
    }

}
