package pulsar;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import gnu.lists.LList;
import gnu.lists.Pair;
import gnu.mapping.Procedure;
import lamu.lib.kawautils.SchemeNamedArgs;
import lamu.lib.kawautils.SchemeValues;
import metro.MetroSequence;
import metro.MetroSequenceFactory;
import metro.MetroTrackFactory;
import metro.MetroTrackMode;
import metro.MetroTrackSynchronizer;
import metro.MetroVoidSequence;

public class PulsarParamReader_newt {
    public static final String PARAM_NAME       = "name";
    public static final String PARAM_TAGS       = "tags";
    public static final String PARAM_SEQ        = "sequence";
    public static final String PARAM_MODE       = "mode";
    public static final String PARAM_START_SYNC = "start-sync";
    public static final String PARAM_STOP_SYNC  = "stop-sync";

    /**
     *  
     * @param object
     * @return
     *    a newly created list which can safely be modified.
     */
    static List<Object> readParamTrackName( Object object ) { 
        object = SchemeValues.schemeNullCheck( object );
        if ( object instanceof Pair ) {
            return new ArrayList<>( (Pair)object );
        } else {
            return new ArrayList<>( Arrays.asList( SchemeValues.schemeNullCheck( object ) ) );
        }
    }

    /**
     * 
     * @param value
     * @return
     */
    static MetroTrackMode readTrackMode(Object value ) {
        return PulsarSchemeValues.fromObject( value );
    }
    
    /**
     * 
     */
    static MetroSequenceFactory readParamSequenceFactory(Object arg) {
        if ( arg  == null ) {
            return MetroVoidSequence.getFactory();
        } else if ( arg instanceof MetroSequence ) {
            return PulsarCommon.createConstantSequenceFactory((MetroSequence)arg);
        } else if ( arg instanceof Procedure ) {
            return PulsarCommon.createDynamicProcedureSequenceFactory((Procedure) arg);
        } else if ( arg  instanceof LList ) {
            return PulsarCommon.createListSequenceFactory((LList)arg);
        } else if ( arg  instanceof Number ) {
            return PulsarCommon.createListSequenceFactory(PulsarCommon.createRestBar(SchemeValues.toInteger(arg)));
        } else {
            throw new IllegalArgumentException( "unsupported type of the argument (" + arg + ")" );
        }
    }
    
    
    // ADDED (Mon, 08 Jun 2020 23:22:13 +0900)
    public static MetroTrackFactory readParamNewTrack(Object[] args0) {
        SchemeNamedArgs args = new SchemeNamedArgs( args0 );
        
        switch ( args.getNumberedArgs().size() ) {
        case 0:
        case 1:
            return MetroTrackFactory.createDefault(
                // 0
                args.getValue(
                    SchemeNamedArgs.<Object>namedRef(
                        PARAM_NAME, 
                        (o)->o,
                        (namedArgs,numberedArgs)->null)),
                // 1
                args.getValue(
                    SchemeNamedArgs.<List<Object>>namedRef(
                        PARAM_TAGS,
                        (o)->((o instanceof Collection) ? (new ArrayList<Object>((Collection)o)) : (new ArrayList<Object>(Arrays.asList(o)))),
                        (namedArgs,numberedArgs)->null)),
                // 2
                args.getValue(
                    SchemeNamedArgs.<MetroSequenceFactory>namedNumberedRef(
                        PARAM_SEQ,
                        0,
                        (o)->readParamSequenceFactory(o),
                        (namedArgs,numberedArgs)->{throw new IllegalArgumentException("no sequence was specified");})),
                args.getValue(
                    SchemeNamedArgs.<MetroTrackMode>namedRef(
                        PARAM_MODE,
                        (o)->readTrackMode(o),
                        (namedArgs,numberedArgs)->null)),
                args.getValue(
                    SchemeNamedArgs.<MetroTrackSynchronizer>namedRef(
                        PARAM_START_SYNC,
                        (o)->(MetroTrackSynchronizer)o,
                        (namedArgs,numberedArgs)->null)), 
                args.getValue(
                    SchemeNamedArgs.<MetroTrackSynchronizer>namedRef(
                        PARAM_STOP_SYNC,
                        (o)->(MetroTrackSynchronizer)o,
                        (namedArgs,numberedArgs)->null)));
        case 2: 
        case 3: 
        case 4: 
            return MetroTrackFactory.createDefault(
                // 0
                args.getValue(
                    SchemeNamedArgs.<Object>namedRef(
                        PARAM_NAME, 
                        (o)->o,
                        SchemeNamedArgs.<Object>numberedRef(
                            0,
                            o->(readParamTrackName(o).get(0)),
                            (namedArgs,numberedArgs)->null))),
                // 1
                args.getValue(
                    SchemeNamedArgs.namedRef(
                        PARAM_TAGS,
                        (o)->((o instanceof Collection) ? (new ArrayList<Object>((Collection)o)) : (new ArrayList<Object>(Arrays.asList(o)))),
                            SchemeNamedArgs.numberedRef(
                                0,
                                (o)->{
                                    List<Object> list = readParamTrackName(o);
                                    list.remove(0);
                                    return list;
                                },
                                (namedArgs,numberedArgs)->null))),
                // 2
                args.getValue(
                    SchemeNamedArgs.namedNumberedRef(
                        PARAM_SEQ,
                        1,
                        (o)->readParamSequenceFactory(o),
                        (namedArgs,numberedArgs)->null)),
                args.getValue(
                    SchemeNamedArgs.namedRef(
                        PARAM_MODE,
                        (o)->readTrackMode(o),
                        (namedArgs,numberedArgs)->null)),
                args.getValue(
                    SchemeNamedArgs.namedNumberedRef(
                        PARAM_START_SYNC,
                        2,
                        (o)->(MetroTrackSynchronizer)o,
                        (namedArgs,numberedArgs)->null)), 
                args.getValue(
                    SchemeNamedArgs.namedNumberedRef(
                        PARAM_STOP_SYNC,
                        3,
                        (o)->(MetroTrackSynchronizer)o,
                        (namedArgs,numberedArgs)->null)));
        default :
            throw new IllegalArgumentException();
        }
    }
}
