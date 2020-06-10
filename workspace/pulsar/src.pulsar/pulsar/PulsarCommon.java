package pulsar;

import java.util.Arrays;
import java.util.List;

import gnu.lists.LList;
import gnu.lists.Pair;
import gnu.mapping.Procedure;
import gnu.mapping.Symbol;
import gnu.math.IntNum;
import metro.MetroPort;
import metro.MetroSequence;
import metro.MetroSequenceFactory;

public class PulsarCommon {
    public static MetroSequenceFactory createListSequenceFactory( LList pair ) {
        return SchemeSequenceFactory.create( PulsarProcedureFactory.createConstantList(pair));
    }
    public static SchemeSequence createSequence( Procedure procedure ){
        return new SchemeSequence( procedure );
    }
    public static MetroSequenceFactory createDynamicProcedureSequenceFactory( Procedure procedure ) {
        return SchemeSequenceFactory.createDynamic( procedure );
    }
    public static MetroSequenceFactory createConstantSequenceFactory( MetroSequence sequence ) {
        return MetroSequenceFactory.createConstant( sequence );
    }
    
    public static MetroSequence createRecordingSequence(  
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
    static LList createRestBar(int intValue) {
        return 
                LList.makeList( Arrays.asList( 
                    LList.makeList( Arrays.asList(
                        Pair.make( Symbol.valueOf( "type" ),  Symbol.valueOf( "len" ) ),
                        Pair.make( Symbol.valueOf( "val" ),   IntNum.valueOf( intValue ))))));
    }

}
