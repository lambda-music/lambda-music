package pulsar;


import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.logging.Level;
import java.util.stream.Collectors;

import gnu.lists.IString;
import gnu.lists.LList;
import gnu.lists.Pair;
import gnu.mapping.Environment;
import gnu.mapping.Procedure;
import gnu.mapping.Symbol;
import lamu.lib.Invokable;
import lamu.lib.helps.LamuDocument;
import lamu.lib.kawautils.SchemeInvokable;
import lamu.lib.kawautils.SchemeValues;
import lamu.lib.kawautils.procedures.MultipleNamedProcedure0;
import lamu.lib.kawautils.procedures.MultipleNamedProcedure1;
import lamu.lib.kawautils.procedures.MultipleNamedProcedureN;
import lamu.lib.logging.Logger;
import metro.Metro;
import metro.MetroPort;
import metro.MetroSequence;
import metro.MetroSequenceFactory;
import metro.MetroSyncType;
import metro.MetroSynchronizable;
import metro.MetroTrack;
import metro.MetroTrackFactory;
import metro.MetroTrackManipulator;
import metro.MetroTrackManipulatorBasic;
import metro.MetroTrackSelector;
import metro.MetroTrackSelectorBasic;
import metro.MetroTrackSynchronizer;
import metro.MetroTrackSynchronizerBasic;
import metro.MetroTrackSynchronizerFactory;
import metro.MetroVoidSequence;

public interface PulsarLib {
    Procedure getGetCurrentPulsar();
    Procedure getIsCurrentPulsarPresent();
    Procedure getIsOpen();
    Procedure getOpen();
    Procedure getClose();
    Procedure getGetClientName();
    Procedure getOpenOutput();
    Procedure getOpenInput();
    Procedure getCloseOutput();
    Procedure getCloseInput();
    Procedure getListOutput();
    Procedure getListInput();
    Procedure getConnect();
    Procedure getDisconnect();
    Procedure getListAllOutput();
    Procedure getListAllInput();
    Procedure getSetMain();
    Procedure getGetMain();
    Procedure getSetPlaying();
    Procedure getIsPlaying();
    Procedure getPlay();
    Procedure getStop();
    Procedure getQuit();
    Procedure getTapTempo();
    Procedure getSetTempo();
    Procedure getGetTempo();
    Procedure getGetBarsPerSecond();
    Procedure getRewind();
    Procedure getExecuteTrack();
//    Procedure getGetTrack();
    Procedure getGetTrackPosition();
//    Procedure getManipulateTrack();
//    Procedure getSelectTrack();
//    Procedure getSyncTrack();
//    Procedure getNewTrack();
//    Procedure getNewRecordingTrack();
//    Procedure getPutTrack();
//    Procedure getRemoveTrack();
//    Procedure getReplaceTrack();
//    Procedure getNotifyTrackChange();
//    Procedure getListTracks();
//    Procedure getClearTracks();
//    Procedure getGetMainTrack();
    public static abstract interface PulsarLibDelegator extends PulsarLib {
        abstract PulsarLib getPulsarLibImplementation();
//        @Override
//        default Procedure getGetMainTrack() {
//            return getPulsarLibImplementation().getGetMainTrack();
//        }
        @Override
        public default Procedure getGetCurrentPulsar() {
            return getPulsarLibImplementation().getGetCurrentPulsar();
        }
        @Override
        public default Procedure getIsCurrentPulsarPresent() {
            return getPulsarLibImplementation().getIsCurrentPulsarPresent();
        }

//        obsolete
//        public default Procedure getClearTracks() {
//            return getPulsarLibImplementation().getClearTracks();
//        }

//        obsolete
//        public default Procedure getListTracks() {
//            return getPulsarLibImplementation().getListTracks();
//        }

//        public default Procedure getNotifyTrackChange() {
//            return getPulsarLibImplementation().getNotifyTrackChange();
//        }

//        public default Procedure getPutTrack() {
//            return getPulsarLibImplementation().getPutTrack();
//        }
//        public default Procedure getRemoveTrack() {
//            return getPulsarLibImplementation().getRemoveTrack();
//        }
//        public default Procedure getReplaceTrack() {
//            return getPulsarLibImplementation().getReplaceTrack();
//        }
//        public default Procedure getNewRecordingTrack() {
//            return getPulsarLibImplementation().getNewRecordingTrack();
//        }
//        public default Procedure getManipulateTrack() {
//            return getPulsarLibImplementation().getManipulateTrack();
//        }
//        public default Procedure getSelectTrack() {
//            return getPulsarLibImplementation().getSelectTrack();
//        }
//        public default Procedure getSyncTrack() {
//            return getPulsarLibImplementation().getSyncTrack();
//        }

//        public default Procedure getNewTrack() {
//            return getPulsarLibImplementation().getNewTrack();
//        }
        default Procedure getExecuteTrack() {
            return getPulsarLibImplementation().getExecuteTrack();
        }
//        public default Procedure getGetTrack() {
//            return getPulsarLibImplementation().getGetTrack();
//        }
        public default Procedure getGetTrackPosition() {
            return getPulsarLibImplementation().getGetTrackPosition();
        }

        public default Procedure getRewind() {
            return getPulsarLibImplementation().getRewind();
        }

        public default Procedure getSetTempo() {
            return getPulsarLibImplementation().getSetTempo();
        }
        public default Procedure getGetTempo() {
            return getPulsarLibImplementation().getGetTempo();
        }
        public default Procedure getGetBarsPerSecond() {
            return getPulsarLibImplementation().getGetBarsPerSecond();
        }

        public default Procedure getTapTempo() {
            return getPulsarLibImplementation().getTapTempo();
        }

        public default Procedure getQuit() {
            return getPulsarLibImplementation().getQuit();
        }

        public default Procedure getStop() {
            return getPulsarLibImplementation().getStop();
        }

        public default Procedure getPlay() {
            return getPulsarLibImplementation().getPlay();
        }

        public default Procedure getIsPlaying() {
            return getPulsarLibImplementation().getIsPlaying();
        }

        public default Procedure getSetPlaying() {
            return getPulsarLibImplementation().getSetPlaying();
        }

        public default Procedure getGetMain() {
            return getPulsarLibImplementation().getGetMain();
        }

        public default Procedure getSetMain() {
            return getPulsarLibImplementation().getSetMain();
        }

        public default Procedure getListAllInput() {
            return getPulsarLibImplementation().getListAllInput();
        }

        public default Procedure getListAllOutput() {
            return getPulsarLibImplementation().getListAllOutput();
        }

        public default Procedure getDisconnect() {
            return getPulsarLibImplementation().getDisconnect();
        }

        public default Procedure getConnect() {
            return getPulsarLibImplementation().getConnect();
        }

        public default Procedure getListInput() {
            return getPulsarLibImplementation().getListInput();
        }

        public default Procedure getListOutput() {
            return getPulsarLibImplementation().getListOutput();
        }

        public default Procedure getCloseInput() {
            return getPulsarLibImplementation().getCloseInput();
        }

        public default Procedure getCloseOutput() {
            return getPulsarLibImplementation().getCloseOutput();
        }

        public default Procedure getOpenInput() {
            return getPulsarLibImplementation().getOpenInput();
        }

        public default Procedure getOpenOutput() {
            return getPulsarLibImplementation().getOpenOutput();
        }

        public default Procedure getGetClientName() {
            return getPulsarLibImplementation().getGetClientName();
        }

        public default Procedure getClose() {
            return getPulsarLibImplementation().getClose();
        }

        public default Procedure getOpen() {
            return getPulsarLibImplementation().getOpen();
        }

        public default Procedure getIsOpen() {
            return getPulsarLibImplementation().getIsOpen();
        }
    }
    
    public static abstract class PulsarLibImplementation implements PulsarLib {
        static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
        static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE,   msg, e   ); }
        static void logInfo (String msg             ) { LOGGER.log(Level.INFO,     msg      ); }
        static void logWarn (String msg             ) { LOGGER.log(Level.WARNING,  msg      ); }
        static final boolean AUTO_EXET = false;

        protected abstract Pulsar getPulsar();

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
        
        
        static double readParamSyncOffset(Object object) {
            return SchemeValues.toDouble( object );
        }
        static MetroSyncType readParamSyncType(Object object) {
            object = SchemeValues.schemeNullCheck(object);
            if ( object == null ) {
                return MetroSyncType.IMMEDIATE;
            } else {
                return MetroSyncType.toSyncType( SchemeValues.toString( object ) );
            }
        }
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
                
        
        protected static List<Object> readParamPortName( Object arg ) {
            if ( arg instanceof Pair ) {
                return ((Pair)arg);
            } else {
                return Arrays.asList( arg );
            }
        }
        protected static List<MetroPort> readParamPort( Object arg, List<MetroPort> portList ) {
            if ( arg instanceof Pair ) {
                List<MetroPort> list = new ArrayList<>();
                for ( Object o : ((Pair)arg) ) {
                    list.addAll( readParamPort( o, portList ) );
                }
                return list;
            } else if ( arg instanceof MetroPort ) {
                return Arrays.asList( (MetroPort)arg );
            } else if ( arg instanceof IString || arg instanceof Symbol ) {
                MetroPort port = readParamNameToPort( portList, arg );
                if ( port == null ) {
                    logWarn( "unsupported type of a value (" + arg + ")" );
                    return Collections.EMPTY_LIST;
                } else {
                    return Arrays.asList( port );
                }
            } else {
                logWarn( "unsupported type of a value (" + arg + ")" );
                return Collections.EMPTY_LIST;
            }
        }
        
        private static MetroPort readParamNameToPort( List<MetroPort> portList, Object arg ) {
            if ( arg instanceof MetroPort ) 
                return (MetroPort)arg;
            
            for ( MetroPort p : portList ) {
                if ( p.getName().equals( arg ) ) 
                    return p;
            }
            return null;
        }


        /**
         * 
         * @param args
         * @return
         */
        public static MetroTrackManipulator readParamMant( String name, Object ... args ) {
        	for ( int i=0; i<args.length; i++ ) {
        		if ( args[i] instanceof Procedure ) {
        			args[i] = new SchemeInvokable( (Procedure) args[i] );
        		}
        	}
            return MetroTrackManipulatorBasic .getFactoryMap() .getFactory(name).create( args );
        }

        
        public static final Object SELT_KEY_NAME    = Symbol.valueOf("name");
        public static final Object SELT_KEY_TAG     = Symbol.valueOf("tag");
        public static final Object SELT_KEY_TAG_OR  = Symbol.valueOf("tag-or");
        public static final Object SELT_KEY_TAG_AND = Symbol.valueOf("tag-and");
        public static final Object SELT_KEY_EXEC    = Symbol.valueOf("exec");
        public static final Object SELT_KEY_CONST   = Symbol.valueOf("const");
        
        private static final Object[] emptyArray = new Object[0];
        public static MetroTrackSelector readParamSelt1(Object value) {
            if ( value == null || Boolean.FALSE.equals(value) ) {
                return MetroTrackSelectorBasic.all();
            } else if ( value instanceof MetroTrackSelector ) {
                return (MetroTrackSelector) value;
            } else if ( Metro.getMainTrackName().equals( value ) ) {
                return MetroTrackSelectorBasic.name(Metro.getMainTrackName());
            } else if ( value instanceof Invokable ) {
                return MetroTrackSelectorBasic.invokable((Invokable)value);
            } else if ( value instanceof Procedure ) {
                return MetroTrackSelectorBasic.invokable( SchemeInvokable.create((Procedure) value) );
            } else {
                return MetroTrackSelectorBasic.getFactoryMap().getFactory( Objects.toString(value)).create(emptyArray);
            }
        }
        public static MetroTrackSelector readParamSelt2( Object[] args ) {
            if ( args.length < 2 )
                throw new IllegalArgumentException( "the number of arguments < 2 " );
            
            String key = SchemeValues.anyToString(args[0]);
            Object[] values = Arrays.copyOfRange(args, 1, args.length );
            return readParamSeltProc(key, values);
        }
		private static MetroTrackSelector readParamSeltProc(String key, Object[] values) {
			return MetroTrackSelectorBasic.getFactoryMap().getFactory(key).create(values);
		}
        
        public static Object readParamSelt2old(Object key, Object value) {
            if ( SELT_KEY_NAME.equals( key ) ) {
                // name
                if ( key instanceof Collection ) {
                    return MetroTrackSelectorBasic.names((Collection)value );
                } else {
                    return MetroTrackSelectorBasic.name(value);
                }
            } else if ( SELT_KEY_TAG_OR.equals( key ) || SELT_KEY_TAG.equals( key ) ) {
                // or
                return MetroTrackSelectorBasic.tagOr((Collection)value );
            } else if ( SELT_KEY_TAG_AND.equals( key ) ) {
                // and
                return MetroTrackSelectorBasic.tagAnd((Collection)value );
            } else if ( SELT_KEY_CONST.equals( key ) ) {
                // const
                if ( key instanceof Collection ) {
                    return MetroTrackSelectorBasic.trackConstant((Collection)value );
                } else {
                    return MetroTrackSelectorBasic.constant((MetroTrack)value );
                }
            } else if ( SELT_KEY_EXEC.equals( key ) ) {
                // exec
                if ( key instanceof Invokable ) {
                    return MetroTrackSelectorBasic.invokable((Invokable)value);
                } else if ( key instanceof Procedure ) {
                    return MetroTrackSelectorBasic.invokable( SchemeInvokable.create((Procedure) value) );
                } else {
                    throw new IllegalArgumentException("unsupported value (" + value + ")" );
                }
            } else {
                throw new IllegalArgumentException("unsupported value (" + value + ")" );
            }
        }
        
        public static MetroTrackSelector readParamSelt( Object[] args ) {
            switch ( args.length  ){
                case 0 : 
                    return MetroTrackSelectorBasic.all();
                case 1 :
                    return readParamSelt1(args[0]);
                default :
                    return readParamSelt2(args);
            }
        }

        // ADDED (Thu, 25 Jun 2020 23:56:05 +0900) >>>
        public static MetroTrackSelector readParamGett( Object[] args ) {
        	return readParamSeltProc("name", args);
        }
        // ADDED (Thu, 25 Jun 2020 23:56:05 +0900) <<<

        
        public static MetroTrackSynchronizer readParamSynct1(Object value) {
            if ( value == null ) {
                return MetroTrackSynchronizerBasic.immediate();
            } else if ( value instanceof MetroTrackSynchronizer ) {
                    return (MetroTrackSynchronizer) value;
            } else if ( value instanceof Procedure ) {
                return PulsarSequencerSynchronizer.create((Procedure)value);
            } else {
                return readParamSynct3( SchemeValues.anyToString( value ) , null, 0.0d  );
            }
        }
        public static MetroTrackSynchronizer readParamSynct3(String syncType, MetroTrackSelector syncTrack, double syncOffset) {
            MetroTrackSynchronizerFactory factory = MetroTrackSynchronizerBasic.getFactoryMap().getFactory(syncType);
            return factory.createSynchronizer( syncTrack, syncOffset );
        }
        public static MetroTrackSynchronizer readParamSynct3( Object[] args ) {
            String syncType   = SchemeValues.anyToString( args[0] );
            MetroTrackSelector syncTrack  = readParamSelt1(args[1]);
            double syncOffset = 2< args.length ? SchemeValues.toDouble(args[2]) : 0.0d;
            return readParamSynct3(syncType, syncTrack, syncOffset);
        }
        public static MetroTrackSynchronizer readParamSynct( Object[] args ) {
            switch ( args.length  ){
                case 0 : 
                    return MetroTrackSynchronizerBasic.immediate();
                case 1 :
                    return readParamSynct1(args[0]);
                case 2 :
                case 3 : {
                    return readParamSynct3(args);
                }
                default :
                    throw new IllegalArgumentException("an unsupported number of arguments (" +args.length+ ")");
            }
        }
        

        /**
         * 
         * @param args
         * @return
         */
        @Deprecated
        public static ArrayList<MetroTrack> readParamTracks( Object[] args ) {
            ArrayList<MetroTrack> tracks = new ArrayList<>();
            for ( Object arg : args ) {
                logInfo("readParamtTrack:" + arg );
                if ( arg instanceof Collection ) {
                    tracks.addAll(((Collection)arg));
                } else if ( arg instanceof MetroTrack ) {
                    tracks.add(((MetroTrack)arg));
                } else {
                    throw new IllegalArgumentException( "unsupported argument error : ( " + arg + ")" );
                }
            }
            return tracks;
        }

        // ADDED (Mon, 08 Jun 2020 23:22:13 +0900)
        public static MetroTrackFactory readParamNewTrack(Object[] args) {
            switch ( args.length ) {
            case 0:
                throw new IllegalArgumentException();
            case 1:
                return MetroTrackFactory.createDefault( 
                    null, 
                    null, 
                    readParamSequenceFactory(args[0]));
            case 2: {
                List<Object> lst = readParamTrackName( args[0] );
                return MetroTrackFactory.createDefault( 
                    lst.remove(0), 
                    lst, 
                    readParamSequenceFactory(args[1]));
            }
            case 3: {
                List<Object> lst = readParamTrackName( args[0] );
                return MetroTrackFactory.createDefault( 
                    lst.remove(0), 
                    lst,
                    readParamSequenceFactory(args[1]),
                    (MetroTrackSynchronizer) args[2],
                    null);
            }
            case 4: {
                List<Object> lst = readParamTrackName( args[0] );
                return MetroTrackFactory.createDefault( 
                    lst.remove(0), 
                    lst,
                    readParamSequenceFactory(args[1]),
                    (MetroTrackSynchronizer) args[2],
                    (MetroTrackSynchronizer) args[3]);
            }
            default :
                throw new IllegalArgumentException();
            }
        }
        public static MetroTrackSelector readParamNewTrackSelector(Object[] args) {
            return MetroTrackSelectorBasic.trackFactory( readParamNewTrack( args ) );
        }
        


        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        //
        //
        //
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        

        static final String THROWS_AN_ERROR_IF_NOT_OPEN = 
            "In case the current sequencer system has not established any connection to the JACK, " + 
                "it throws an exception. ";

        static final String ALTERS_THE_CURRENT_STATE =
            "This procedure alters the current sequencer system's state. ";
        
        public static abstract class PulsarProceduralDescriptiveDoc extends LamuDocument {
            public PulsarProceduralDescriptiveDoc() {
            }
        }

        public static final CurrentPulsarDoc currentPulsarDoc = new CurrentPulsarDoc();
        public static final class CurrentPulsarDoc extends PulsarProceduralDescriptiveDoc {
            {
                setCategory( Pulsar.DOCS_ID );
                setNames( "current-pulsar" );
                setParameterDescription( "" );
                setReturnValueDescription( "" );
                setShortDescription(       "" );
                setLongDescription(        ""
                                         + "" );
            }
        }

        public final PulsarProc currentPulsarProc = new PulsarProc(new String[] { "current-pulsar" });
        @Override
        public final Procedure getGetCurrentPulsar() { return  currentPulsarProc; }
        public final class PulsarProc extends MultipleNamedProcedure0 {
            public PulsarProc(String[] names) {
                super(names);
            }

            @Override
            public Object apply0() throws Throwable {
                return getPulsar();
            }
        }
        

        public static final CurrentPulsarPresentDoc isCurrentPulsarPresentDoc = new CurrentPulsarPresentDoc();
        public static final class CurrentPulsarPresentDoc extends PulsarProceduralDescriptiveDoc {
            {
                setCategory( Pulsar.DOCS_ID );
                setNames( "current-pulsar-present?" );
                setParameterDescription( "" );
                setReturnValueDescription( "" );
                setShortDescription(       "" );
                setLongDescription(        ""
                                         + "" );
            }
        }

        public final IsPulsarPresentProc isCurrentPulsarPresentProc = new IsPulsarPresentProc(new String[] { "current-pulsar-present?" });
        @Override
        public Procedure getIsCurrentPulsarPresent() { return isCurrentPulsarPresentProc ; }
        public final class IsPulsarPresentProc extends MultipleNamedProcedure0 {
            public IsPulsarPresentProc(String[] names) {
                super(names);
            }

            @Override
            public Object apply0() throws Throwable {
                return getPulsar() !=null;
            }
        }


        public static final IsOpenDoc isOpenDoc = new IsOpenDoc();
        public static final class IsOpenDoc extends PulsarProceduralDescriptiveDoc {
            {
                setCategory( Pulsar.DOCS_ID );
                setNames( "open?" );
                setParameterDescription( "" );
                setReturnValueDescription( "::boolean" );
                setShortDescription(       "||<name/>|| returns the current open state. " );
                setLongDescription(        "This procedure returns #t iff the current sequencer state is open; "
                                         + "otherwise returns #f. " );
            }
        }

        public final IsOpenProc isOpenProc = new IsOpenProc(new String[] { "open?" });
        @Override
        public Procedure getIsOpen () { return isOpenProc ; }
        public final class IsOpenProc extends MultipleNamedProcedureN {
            public IsOpenProc(String[] names) {
                super(names);
            }

            @Override
            public Object applyN(Object[] args) throws Throwable {
                return getPulsar().isOpened();
            }
        }


        public static final OpenDoc openDoc = new OpenDoc();
        public static final class OpenDoc extends PulsarProceduralDescriptiveDoc {
            {
                setCategory( Pulsar.DOCS_ID );
                setNames( "open" );
                setParameterDescription( "[string]" );
                addParameter( 0, "client-name", "string", null , false, "The client name in the current Jack session. " );
                setReturnValueDescription( "::void" );
                setShortDescription( "starts a new connection between JACK Audio Connection Kit." );
                setLongDescription( 
                          "This procedure opens a new connection to the installed JACK Audio Connection Kit with"
                        + "the specified client name. "
                        + "When it failed to open a connection, this throws an exception. "
                        + ALTERS_THE_CURRENT_STATE );
            }
        }

        public final OpenProc openProc = new OpenProc(new String[] { "open" });
        @Override
        public Procedure getOpen() { return openProc; }
        public final class OpenProc extends MultipleNamedProcedure1 {
            public OpenProc(String[] names) {
                super(names);
            }

            @Override
            public Object apply1(Object arg0) throws Throwable {
                getPulsar().open( SchemeValues.toString( arg0 ) );
                return SchemeValues.NO_RESULT;
            }
        }


        public static final CloseDoc closeDoc = new CloseDoc();
        public static final class CloseDoc extends PulsarProceduralDescriptiveDoc {
            {
                setCategory( Pulsar.DOCS_ID );
                setNames( "close" );
                setParameterDescription( "" );
                setReturnValueDescription( "::void" );
                setShortDescription( "ends the current connection between JACK Audio Connection Kit." );
                setLongDescription( "This procedure closes the current connection to the JACK Audio Connection Kit. "
                        + "When it failed to close the connection, this throws an exception. "
                        + THROWS_AN_ERROR_IF_NOT_OPEN
                        + ALTERS_THE_CURRENT_STATE );
            }
        }

        public final CloseProc closeProc = new CloseProc(new String[] { "close" });
        @Override
        public Procedure getClose() { return closeProc; }
        public final class CloseProc extends MultipleNamedProcedureN {
            public CloseProc(String[] names) {
                super(names);
            }

            @Override
            public Object applyN(Object[] args) throws Throwable {
                getPulsar().close();
                return SchemeValues.NO_RESULT;
            }
        }

        public final GetClientNameProc getClientNameProc = new GetClientNameProc(new String[] { "get-client-name" });
        @Override
        public Procedure getGetClientName() { return getClientNameProc; }
        public final class GetClientNameProc extends MultipleNamedProcedureN {
            public GetClientNameProc(String[] names) {
                super(names);
            }
            @Override
            public Object applyN(Object[] args) throws Throwable {
                return SchemeValues.toSchemeString(getPulsar().getClientName());
            }
        }

        public static final PulsarProceduralDescriptiveDoc openPortTemplateDoc = new OpenPortTemplateDoc(); 
        public static final class OpenPortTemplateDoc extends PulsarProceduralDescriptiveDoc {{
                setVisible(false);
                setCategory( Pulsar.DOCS_ID );
                setParameterDescription( "[ANY|(list ANY...)  ]..." );
                addParameter( 0, "port-name", "any|(list any ...)", null , true, "The port name in the current JACK session. " );
                setReturnValueDescription( "::MetroPort" );
                setShortDescription( "opens %s ports on the current JACK connection. " );
                setLongDescription( "" + "Each argument is the name of a port to create. "
                        + "The value can be a value of any type; thought, it is usually a value "
                        + "which is easy to be distinguished such as a symbol value or a string value. "
                        + "The value is applied as the identifier of the created port. "
                        + "A duplicated port name on the current JACK connection causes an exception to be thrown. "
                        + THROWS_AN_ERROR_IF_NOT_OPEN
                        + ALTERS_THE_CURRENT_STATE
            );
        }}

        public static final LamuDocument openOutputDoc = ( openPortTemplateDoc.processArguments( "output" ).setNames( "open-output", "openo" ) );

        public static void main(String[] args) {
            System.out.println(openOutputDoc );
        }

        public final Procedure openOutputProc = new OpenOutputProc(new String[] { "open-output", "openo" });
        @Override
        public Procedure getOpenOutput() { return openOutputProc; }
        public final class OpenOutputProc extends MultipleNamedProcedureN {
            public OpenOutputProc(String[] names) {
                super(names);
            }

            @Override
            public Object applyN(Object[] args) throws Throwable {
                Pulsar pulsar = getPulsar();
                ArrayList<MetroPort> list = new ArrayList<>();
                for ( Object o : args ) {
                    for ( Object portName : readParamPortName( o ) ) {
                        list.add( pulsar.createOutputPort(portName) );
                    }
                }
                Collections.reverse( list );
                return LList.makeList( list );
            }
        }

        public static final LamuDocument openInputDoc =  openPortTemplateDoc.processArguments( "input" ).setNames( "open-input" , "openi" );

        public final Procedure openInputProc = new OpenInputProc(new String[] { "open-input", "openi" });
        @Override
        public Procedure getOpenInput() { return openInputProc; }
        public final class OpenInputProc extends MultipleNamedProcedureN {
            public OpenInputProc(String[] names) {
                super(names);
            }

            @Override
            public Object applyN(Object[] args) throws Throwable {
                Pulsar pulsar = getPulsar();
                ArrayList<MetroPort> list = new ArrayList<>();
                for ( Object o : args ) {
                    for ( Object portName : readParamPortName( o ) ) {
                        list.add( pulsar.createInputPort(portName) );
                    }
                }
                Collections.reverse( list );
                return LList.makeList( list );
            }
        }

        public static final ClosePortTemplateDoc closePortTemplateDoc =  new ClosePortTemplateDoc();
        public static final class ClosePortTemplateDoc extends PulsarProceduralDescriptiveDoc {{
            setVisible(false);
            setCategory( Pulsar.DOCS_ID );
            setParameterDescription( "[MetroPort|symbol|string|(list MetroPort|symbol|string ...) ]..." );
            addParameter( 0, "port", "MetroPort|symbol|string|(list MetroPort|symbol|string ...)", null , true, "The port object to close. " );
            setReturnValueDescription( "::void" );
            setShortDescription( "closes the specified %s ports on the current JACK connection. " );
            setLongDescription(  
                "Each argument is a reference to a MetroPort object to close. "
                        + "A value which is other than a reference to a MetroPort object is treated "
                        + "as an identifier of a MetroPort object and automatically "
                        + "replaced with a reference value to the corresponding MetroPort object. "
                        + "The value is applied as the identifier of the created port. "
                        + "A duplicated port name on the current JACK connection causes an exception to be thrown. "
                        + THROWS_AN_ERROR_IF_NOT_OPEN
                        + ALTERS_THE_CURRENT_STATE );
        }}

        public static final LamuDocument closeOutputDoc =  closePortTemplateDoc.processArguments( "output" ).setNames( "close-output" , "closeo" );

        public final Procedure closeOutputProc = new CloseOutputProc(new String[] { "close-output", "closeo" });
        @Override
        public Procedure getCloseOutput() { return closeOutputProc; }
        public final class CloseOutputProc extends MultipleNamedProcedureN {
            public CloseOutputProc(String[] names) {
                super(names);
            }

            @Override
            public Object applyN(Object[] args) throws Throwable {
                Pulsar pulsar = getPulsar();
                for ( Object o : args ) {
                    for ( MetroPort p : readParamPort( o, pulsar.getOutputPorts() ) ) {
                        pulsar.destroyOutputPort( p );
                    }
                }
                return SchemeValues.NO_RESULT;
            }
        }

        public static final LamuDocument closeInputDoc =  closePortTemplateDoc.processArguments( "input" ).setNames( "close-input", "closei" );

        public final Procedure closeInputProc = new CloseInputProc(new String[] { "close-input", "closei" });
        @Override
        public Procedure getCloseInput() { return closeInputProc; }
        public final class CloseInputProc extends MultipleNamedProcedureN {
            public CloseInputProc(String[] names) {
                super(names);
            }

            @Override
            public Object applyN(Object[] args) throws Throwable {
                Pulsar pulsar = getPulsar();
                for ( Object o : args ) {
                    for ( MetroPort p : readParamPort( o, pulsar.getInputPorts() ) ) {
                        pulsar.destroyInputPort( p );
                    }
                }
                return SchemeValues.NO_RESULT;
            }
        }

        public static final ListPortsTemplateDoc listPortsTemplateDoc =  new ListPortsTemplateDoc();
        public static final class ListPortsTemplateDoc extends PulsarProceduralDescriptiveDoc {{
            setVisible(false);
            setCategory( Pulsar.DOCS_ID );
            setParameterDescription( "" );
            setReturnValueDescription( "::(list MetroPort ...)" );
            setShortDescription( "returns a list which contains all %s ports on the current JACK connection. " );
            setLongDescription( ""
                                + "Each element on the list is a reference to a MetroPort object. "
                                + "The values in the list are sorted from newest to oldest. "
                                + THROWS_AN_ERROR_IF_NOT_OPEN
                                );
        }}

        public static final LamuDocument listOutputDoc =  listPortsTemplateDoc.processArguments( "output" ).setNames("list-output" , "lso" );

        public final Procedure listOutputProc = new ListOutputProc(new String[] { "list-output", "lso" });
        @Override
        public Procedure getListOutput() { return listOutputProc; }
        public final class ListOutputProc extends MultipleNamedProcedureN {
            public ListOutputProc(String[] names) {
                super(names);
            }

            @Override
            public Object applyN(Object[] args) throws Throwable {
                List<MetroPort> list = getPulsar().getOutputPorts();
                Collections.reverse( list );
                return LList.makeList( list  );
            }
        }

        public static final LamuDocument listInputDoc =  listPortsTemplateDoc.processArguments( "input" ).setNames("list-input" , "lsi");

        public final Procedure listInputProc = new ListInputProc(new String[] { "list-input", "lsi" });
        @Override
        public Procedure getListInput() { return listInputProc; }
        public final class ListInputProc extends MultipleNamedProcedureN {
            public ListInputProc(String[] names) {
                super(names);
            }

            @Override
            public Object applyN(Object[] args) throws Throwable {
                List<MetroPort> list = getPulsar().getInputPorts();
                Collections.reverse( list );
                return LList.makeList( list  );
            }
        }

        public static final ConnectionTemplateDoc connectTemplateDoc =  new ConnectionTemplateDoc();
        public static final class ConnectionTemplateDoc extends PulsarProceduralDescriptiveDoc {{
            setVisible(false);
            setCategory( Pulsar.DOCS_ID );
            setParameterDescription( "[string] ..." );
            addParameter( 0, "from", "string", null , true, "a canonical port name in the current JACK session. " );
            addParameter( 0, "to", "string", null , true, "a canonical port name in the current JACK session. " );
            setReturnValueDescription( "::void" );
            setShortDescription( "%s specified two ports on the current JACK connection. " );
            setLongDescription( ""
                                + "This procedure %1$s "
                                + "the port which is specified in the first argument to "
                                + "the port which is specified in the second argument. "
                                + "The rest arguments are also processed in the same manner; that is "
                                + "this procedure %1$s each port in the argument which position is in odd ordinal number, "
                                + "to the port in the argument which position is in even ordinal number. \n\n"
                                + "A canonical port name consists two parts; these are separated by a semicolon and "
                                + "the former part is the name of a client and the latter is the name of a port "
                                + "as \"a-nice-client:the-port\". \n\n"
                                + "It is able to enumerate all ports by ||list-all-output|| and ||list-all-input|| procedure. "
                                + ""
                                + THROWS_AN_ERROR_IF_NOT_OPEN );
        }}

        public static final LamuDocument connectDoc =  connectTemplateDoc.processArguments( "connects" ).setNames( "connect" );

        public final ConnectProc connectProc = new ConnectProc(new String[] { "connect" });
        @Override
        public Procedure getConnect() { return connectProc; }
        public final class ConnectProc extends MultipleNamedProcedureN {
            public ConnectProc(String[] names) {
                super(names);
            }

            @Override
            public Object applyN(Object[] args) throws Throwable {
                Pulsar.connectProc( getPulsar(), args, Pulsar.ConnectProcedure.CONNECT );
                return SchemeValues.NO_RESULT;
            }
        }

        public final LamuDocument disconnectDoc =  connectTemplateDoc.processArguments( "disconnects" ).setNames( "disconnect" );

        public final DisconnectProc disconnectProc = new DisconnectProc(new String[] { "disconnect" });
        @Override
        public Procedure getDisconnect() { return disconnectProc; }
        public final class DisconnectProc extends MultipleNamedProcedureN {
            public DisconnectProc(String[] names) {
                super(names);
            }

            @Override
            public Object applyN(Object[] args) throws Throwable {
                Pulsar.connectProc(getPulsar(), args, Pulsar.ConnectProcedure.DISCONNECT );
                return SchemeValues.NO_RESULT;
            }
        }

        public static final InitDocAllConnectionDoc allConnectionTemplateDoc =  new InitDocAllConnectionDoc(); 
        public static class InitDocAllConnectionDoc extends PulsarProceduralDescriptiveDoc {{
            setVisible(false);
            setCategory( Pulsar.DOCS_ID );
            setParameterDescription( "" );
            setReturnValueDescription( "::list<string>" );
            setShortDescription( "retrieves IDs of all %s connections in the current session of JACK Audio Connection Kit. " );
            setLongDescription( ""
                                + "Each ID contains two parts which are separated by a separator character \":\"; "
                                + "the former part is the server name part and the latter part is the port name part."
                                + "The passed arguments are silently discarded. "
                                + THROWS_AN_ERROR_IF_NOT_OPEN
                                );
        }}

        public static final LamuDocument listAllOutputDoc =  allConnectionTemplateDoc.processArguments( "output" ).setNames("list-all-output", "lao");
        public final ListAllOutputProc listAllOutputProc = new ListAllOutputProc(new String[] { "list-all-output", "lao" });
        @Override
        public Procedure getListAllOutput() { return listAllOutputProc; }
        public final class ListAllOutputProc extends MultipleNamedProcedureN {
            public ListAllOutputProc(String[] names) {
                super(names);
            }

            @Override
            public Object applyN(Object[] args) throws Throwable {
                return Pair.makeList( getPulsar().getAvailableOutputPorts().stream().map( (v)->SchemeValues.toSchemeString(v) )
                    .collect( Collectors.toList() ) );
            }
        }

        public static final LamuDocument listAllInputDoc =  allConnectionTemplateDoc.processArguments( "input" ).setNames("list-all-input", "lai");

        public final ListAllInputProc listAllInputProc = new ListAllInputProc(new String[] { "list-all-input", "lai" });
        @Override
        public Procedure getListAllInput() { return listAllInputProc; }
        public final class ListAllInputProc extends MultipleNamedProcedureN {
            public ListAllInputProc(String[] names) {
                super(names);
            }

            @Override
            public Object applyN(Object[] args) throws Throwable {
                return Pair.makeList( getPulsar().getAvailableInputPorts().stream().map( (v)->SchemeValues.toSchemeString(v) )
                    .collect( Collectors.toList() ) );
            }
        }

        public final SetMainProc setMainProc = new SetMainProc(new String[] { "set-main" });
        @Override
        public Procedure getSetMain() { return setMainProc; }
        public final class SetMainProc extends MultipleNamedProcedureN {
            public SetMainProc(String[] names) {
                super(names);
            }

            @Override
            public Object applyN(Object[] args) throws Throwable {
                logInfo("set-main");
                if ( args.length == 1 ) {
                    Procedure procedure = (Procedure)args[0];
                    getPulsar().setMainProcedure( SchemeInvokable.create( procedure ) );
                } else {
                    throw new RuntimeException( "invalid argument length" );
                }
                return SchemeValues.NO_RESULT;
            }
        }

        public static final SetMainDoc setMainDoc =  new SetMainDoc();
        public static final class SetMainDoc extends PulsarProceduralDescriptiveDoc {
            {
                setCategory( Pulsar.DOCS_ID );
                setNames( "set-main" );
                setParameterDescription( "[procedure]" );
                addParameter( 0, "main-procedure", "procedure", null , false, "a procedure to set as the main procedure. " );
                setReturnValueDescription( "::void" );
                setShortDescription( "sets the main procedure. " );
                setLongDescription( ""
                                    + "The main procedure is a procedure which is called "
                                    + "when (rewind) procedure is called in order to reset the sequencer's state. "
                                    + "Usually, the main procedure is a procedure to boot up the current song system. "
                                );
            }
        }

        ///////////////////////////////////////////////////////

        public final GetMainProc getMainProc = new GetMainProc(new String[] { "get-main" });
        @Override
        public Procedure getGetMain() { return getMainProc; }
        public final class GetMainProc extends MultipleNamedProcedure0 {
            public GetMainProc(String[] names) {
                super(names);
            }

            @Override
            public Object apply0() throws Throwable {
                return getPulsar().getMainProcedure();
            }
        }

        public static final GetMainDoc getMainDoc =  new GetMainDoc();
        public static final class GetMainDoc extends PulsarProceduralDescriptiveDoc {
            {
                setCategory( Pulsar.DOCS_ID );
                setNames( "get-main" );
                setParameterDescription( "" );
                setReturnValueDescription( "::procedure" );
                setShortDescription( "retrieves the main procedure. " );
                setLongDescription( ""
                        + "See (help set-main) for further information. " );
            }
        }

        public final SetPlayingProc setPlayingProc = new SetPlayingProc(new String[] { "set-playing", "p" });
        @Override
        public Procedure getSetPlaying() { return setPlayingProc; }
        public final class SetPlayingProc extends MultipleNamedProcedureN {
            public SetPlayingProc(String[] names) {
                super(names);
            }

            @Override
            public Object applyN(Object[] args) throws Throwable {
                Pulsar pulsar = getPulsar();
                if ( args.length == 0 ) {
                    pulsar.togglePlaying();
                } else if ( args.length == 1 ) {
                    pulsar.setPlaying( (Boolean)args[0] );
                } else {
                    throw new RuntimeException( "invalid argument length" );
                }
                return SchemeValues.NO_RESULT;
            }
        }

        public static final SetPlayingDoc setPlayingDoc =  new SetPlayingDoc();
        public static final class SetPlayingDoc extends PulsarProceduralDescriptiveDoc {
            {
                setCategory( Pulsar.DOCS_ID );
                setNames("set-playing" );
                setParameterDescription( "[boolean]" );
                addParameter( 0, "playing","boolean",  null, false, "the status to set. " );
                setReturnValueDescription( "::void" );
                setShortDescription( "sets the current playing state." );
                setLongDescription( ""
                                    + "When #f is passed to this procedure, the sequencer stops playing.  "
                                    + "When #t is passed to this procedure ,the sequencer resumes playing. "
                                    + THROWS_AN_ERROR_IF_NOT_OPEN
                                     );
            }
        }


        public final IsPlayingProc isPlayingProc = new IsPlayingProc(new String[] { "playing?" });
        @Override
        public Procedure getIsPlaying() { return isPlayingProc; }
        public final class IsPlayingProc extends MultipleNamedProcedureN {
            public IsPlayingProc(String[] names) {
                super(names);
            }

            @Override
            public Object applyN(Object[] args) throws Throwable {
                return getPulsar().getPlaying();
            }
        }

        public static final IsPlayingDoc isPlayingDoc =  new IsPlayingDoc();
        public static final class IsPlayingDoc extends PulsarProceduralDescriptiveDoc {
            {
                setCategory( Pulsar.DOCS_ID );
                setNames( "playing?" );
                setParameterDescription( "" );
                setReturnValueDescription( "::boolean" );
                setShortDescription( "retrieves the current playing state." );
                setLongDescription( ""
                                    + "When the sequencer is playing, it returns #t; otherwise it returns #f. "
                                    + "See (help set-playing) for further information. "
                                    + THROWS_AN_ERROR_IF_NOT_OPEN
                                     );
            }
        }

        public final PlayProc playProc = new PlayProc(new String[] { "play" });
        @Override
        public Procedure getPlay() { return playProc; }
        public final class PlayProc extends MultipleNamedProcedureN {
            public PlayProc(String[] names) {
                super(names);
            }

            @Override
            public Object applyN(Object[] args) throws Throwable {
                getPulsar().setPlaying( true ); 
                return SchemeValues.NO_RESULT;
            }
        }

        public static final PlayDoc playDoc =  new PlayDoc();
        public static final class PlayDoc extends PulsarProceduralDescriptiveDoc {
            {
                setCategory( Pulsar.DOCS_ID );
                setNames( "play" );
                setParameterDescription( "" );
                setReturnValueDescription( "::void" );
                setShortDescription( "causes the sequencer to start playing." );
                setLongDescription( "See (help set-play) for further information."
                                    + THROWS_AN_ERROR_IF_NOT_OPEN
                                     );
            }
        }

        public final StopProc stopProc = new StopProc(new String[] { "stop" });
        @Override
        public Procedure getStop() { return stopProc; }
        public final class StopProc extends MultipleNamedProcedureN {
            public StopProc(String[] names) {
                super(names);
            }

            @Override
            public Object applyN(Object[] args) throws Throwable {
                getPulsar().setPlaying( false ); 
                return SchemeValues.NO_RESULT;
            }
        }

        public static final StopDoc stopDoc =  new StopDoc();
        public static final class StopDoc extends PulsarProceduralDescriptiveDoc {
            {
                setCategory( Pulsar.DOCS_ID );
                setNames( "stop"  );
                setParameterDescription( "" );
                setReturnValueDescription( "::void" );
                setShortDescription( "causes the sequencer to stop playing." );
                setLongDescription( 
                    "See (help set-play) for further information."
                        + THROWS_AN_ERROR_IF_NOT_OPEN );
            }
        }

        public final QuitProc quitProc = new QuitProc(new String[] { "quit" });
        @Override
        public Procedure getQuit() { return quitProc; }
        public final class QuitProc extends MultipleNamedProcedureN {
            public QuitProc(String[] names) {
                super(names);
            }

            @Override
            public Object applyN(Object[] args) throws Throwable {
                long shutdownWaitNow;
                if ( 0 < args.length  ) {
                    shutdownWaitNow = SchemeValues.toLong( args[0] );
                } else {
                    shutdownWaitNow = Pulsar.shutdownWait;
                }
                
                Pulsar pulsar = getPulsar();
                Thread t = new Thread() {
                    @Override
                    public void run() {
                        try {
                            Thread.sleep( shutdownWaitNow );
                        } catch (InterruptedException e) {
                            logWarn( e.getMessage() );
                        }
                        pulsar.getParentApplicationComponent().processQuit(); 
                    }
                };
                t.start();
                
                return "Now Pulsar will shutdown in " + shutdownWaitNow + " milliseconds...";
            }
        }

        public static final QuitDoc quitDoc =  new QuitDoc();
        public static final class QuitDoc extends PulsarProceduralDescriptiveDoc {
            {
                setCategory( Pulsar.DOCS_ID );
                setNames( "quit"  );
                setParameterDescription( "" );
                setReturnValueDescription( "::void" );
                setShortDescription( "quits the application. " );
                setLongDescription( ""
                                    + "makes the sequencer to stop playing "
                                    + "and shutdowns the application in " + Pulsar.shutdownWait + " milliseconds. "
                                    + "Currently the time to shutdown is hard-coded and cannot be changed. "
                                    + THROWS_AN_ERROR_IF_NOT_OPEN );
            }
        }

        public final TapTempoProc tapTempoProc = new TapTempoProc(new String[] { "tap-tempo", "tapt" });
        @Override
        public Procedure getTapTempo() { return tapTempoProc; }
        public final class TapTempoProc extends MultipleNamedProcedureN {
            public TapTempoProc(String[] names) {
                super(names);
            }

            @Override
            public Object applyN(Object[] args) throws Throwable {
                logInfo( "Pulsar Scheme API: TAP-TEMPO" );
                Pulsar pulsar = getPulsar();
                double bpm = pulsar.getTempoTapper().tap( pulsar.getBeatsPerMinute() );
                return SchemeValues.toSchemeNumber( bpm );
            }
        }

        public static final TapTempoDoc tapTempoDoc =  new TapTempoDoc();
        public static final class TapTempoDoc extends PulsarProceduralDescriptiveDoc {
            {
                setCategory( Pulsar.DOCS_ID );
                setNames( "tap-tempo", "tapt" );
                setParameterDescription( "" );
                setReturnValueDescription( "::void" );
                setShortDescription( "has the same effect with pressing the tap-tempo button on the main screen. "
                                    );
                setLongDescription( ""
                                    + "(Tue, 07 Jan 2020 01:19:46 +0900) Note : this description is obsolete. \n"
                                    + "The tap-tempo button is a button to change the current tempo. "
                                    + "The button is supposed to be pressed repeatedly to tell the system  "
                                    + "how fast the sequencer should play the current music. "
                                    + "The sequencer calculates the average of durations between the pressing the button, "
                                    + "and apply the value as the current tempo on the sequencer system. "
                                    + "See (help set-tempo)."
                                    + THROWS_AN_ERROR_IF_NOT_OPEN );
            }
        }

        
        public final GetTempoProc getTempoProc = new GetTempoProc(new String[] { "get-tempo", "getbpm" });
        @Override
        public Procedure getGetTempo() { return getTempoProc; }
        public final class GetTempoProc extends MultipleNamedProcedureN {
            public GetTempoProc(String[] names) {
                super(names);
            }
            @Override
            public Object applyN(Object[] args) throws Throwable {
                return getPulsar().getBeatsPerMinute();
            }
        }

        public static final GetTempoDoc getTempoDoc =  new GetTempoDoc();
        public static final class GetTempoDoc extends PulsarProceduralDescriptiveDoc {
            {
                setCategory( Pulsar.DOCS_ID );
                setNames( "get-tempo" );
                setParameterDescription( "" );
                setReturnValueDescription( "::number" );
                setShortDescription( "gets the current tempo. " );
                setLongDescription( ""
                                    + "This procedure returns the value of current tempo as a beat-per-minutes value. "
                                    + "See (help tap-tempo) for further information."
                                    + THROWS_AN_ERROR_IF_NOT_OPEN );
            }
        }

        
        public final SetTempoProc setTempoProc = new SetTempoProc(new String[] { "set-tempo", "setbpm" });
        @Override
        public Procedure getSetTempo() { return setTempoProc; }
        public final class SetTempoProc extends MultipleNamedProcedureN {
            public SetTempoProc(String[] names) {
                super(names);
            }

            @Override
            public Object applyN(Object[] args) throws Throwable {
                if ( 0 < args.length ) {
                    double bpm = SchemeValues.toDouble(args[0]);
                    getPulsar().setBeatsPerMinute( bpm );
                }
       
                return SchemeValues.NO_RESULT;
            }
        }

        public static final SetTempoDoc setTempoDoc =  new SetTempoDoc();
        public static final class SetTempoDoc extends PulsarProceduralDescriptiveDoc {
            {
                setCategory( Pulsar.DOCS_ID );
                setNames( "set-tempo" );
                setParameterDescription( "number" );
                addParameter( 0, "tempo", "number", null, false, "the tempo to set." ); 
                setReturnValueDescription( "::void" );
                setShortDescription( "sets the current tempo. " );
                setLongDescription( ""
                                    + "This procedure takes an argument as a beat-per-minutes. "
                                    + "A value less than one is treated as one. "
                                    + "There is no maximum value for the argument. "
                                    + "thought, the result of passing extreme values to the procedure "
                                    + "is not defined. \n\n"
                                    + "See (help tap-tempo) for further information."
                                    + THROWS_AN_ERROR_IF_NOT_OPEN );
            }
        }


        public final GetBarsPerSecondProc getBarsPerSecondProc = new GetBarsPerSecondProc(new String[] { "get-bars-per-second", "bps" });
        @Override
        public Procedure getGetBarsPerSecond() { return getBarsPerSecondProc; }
        public final class GetBarsPerSecondProc extends MultipleNamedProcedureN {
            public GetBarsPerSecondProc(String[] names) {
                super(names);
            }
            @Override
            public Object applyN(Object[] args) throws Throwable {
                return SchemeValues.toSchemeNumber( getPulsar().getBarsPerSecond() );
            }
        }

        public static final GetBarsPerSecondDoc getSecondDoc =  new GetBarsPerSecondDoc();
        public static final class GetBarsPerSecondDoc extends PulsarProceduralDescriptiveDoc {
            {
                setCategory( Pulsar.DOCS_ID );
                setNames( "get-bars-per-second" );
                setParameterDescription( "" );
                setReturnValueDescription( "::number" );
                setShortDescription( "gets the current tempo. " );
                setLongDescription( ""
                                    + "This procedure returns the value of current tempo as a beat-per-minutes value. "
                                    + "See (help tap-tempo) for further information."
                                    + THROWS_AN_ERROR_IF_NOT_OPEN );
            }
        }

        
        public final RewindProc rewindProc = new RewindProc(new String[] { "rewind" });
        @Override
        public Procedure getRewind() { return rewindProc; }
        public final class RewindProc extends MultipleNamedProcedure0 {
            public RewindProc(String[] names) {
                super(names);
            }

            @Override
            public Object apply0() throws Throwable {
                getPulsar().rewind();
                return SchemeValues.NO_RESULT;
            }
        }

        public static final RewindDoc rewindDoc =  new RewindDoc();
        public static final class RewindDoc extends PulsarProceduralDescriptiveDoc {
            {
                setCategory( Pulsar.DOCS_ID );
                setNames( "rewind" );
                setParameterDescription( "" );
                setReturnValueDescription( "::void" );
                setShortDescription( "causes the music sequencer to go to the head of the song. " );
                setLongDescription( ""
                                    + "This procedure usually causes the music sequencer to call the main-procedure. "
                                    + "See (help set-main). "
                                    + THROWS_AN_ERROR_IF_NOT_OPEN );
            }
        }

        public static void exetProcParse(
            Collection<? extends Object> argList,
            List<MetroTrackSelector> selectors,
            List<MetroTrackManipulator> manipulators )
        {
            for ( Object o : argList ) {
                if ( o instanceof MetroTrackSelector ) {
                    selectors.add((MetroTrackSelector) o);
                } else if ( o instanceof MetroTrackManipulator ) {
                    manipulators.add((MetroTrackManipulator) o);
                } else if ( o instanceof Collection ) {
                    exetProcParse( (Collection<? extends Object>)o, selectors, manipulators);
                } else {
                    logWarn( "an unsupported object was detected and ignored " + o );
                }
            }
        }

        public static List<MetroTrack> exetProcCall(
            Metro metro,
            List<MetroTrackSelector> trackSelectors,
            List<MetroTrackManipulator> trackManipulators,
            List<MetroTrack> selectedTracks) 
        {
            if (!trackManipulators.isEmpty())
                metro.manipulateTrack(trackManipulators);

            if (!trackSelectors.isEmpty()) {
                metro.referTracks(trackSelectors, selectedTracks);
            }
            return selectedTracks;
        }
        
        public static void exetProc(
            Metro metro,
            List<Object> argList,
            List<MetroTrackSelector> trackSelectors,
            List<MetroTrackManipulator> trackManipulators,
            List<MetroTrack> selectedTracks) 
        {
            exetProcParse(argList, trackSelectors, trackManipulators);
            exetProcCall(metro, trackSelectors, trackManipulators, selectedTracks);
        }

        public static List<MetroTrack> exetProc_no_shared( Metro metro, List<Object> argList ) {
            List<MetroTrackSelector>    trackSelectors    = new ArrayList<>();
            List<MetroTrackManipulator> trackManipulators = new ArrayList<>();
            List<MetroTrack>            selectedTracks    = new ArrayList<>(); // output
            exetProc(metro, argList, trackSelectors, trackManipulators, selectedTracks);
            return selectedTracks;
        }

        
        public final Procedure executeTrackProc = new ExecuteTrackProc(new String[] { "execute-track", "exet" });
        @Override
        public Procedure getExecuteTrack() { return executeTrackProc; }
        public final class ExecuteTrackProc extends MultipleNamedProcedureN {
            public ExecuteTrackProc(String[] names) {
                super(names);
            }
            private final List<MetroTrackSelector>    trackSelectors    = new ArrayList<>();
            private final List<MetroTrackManipulator> trackManipulators = new ArrayList<>();
            private final List<MetroTrack>            selectedTracks    = new ArrayList<>(); // output

            @Override
            public synchronized Object applyN(Object[] args) throws Throwable {
                trackSelectors.clear();
                trackManipulators.clear();
                selectedTracks.clear();
                Metro metro = getPulsar();
                exetProc( metro, Arrays.asList(args), trackSelectors, trackManipulators, selectedTracks );
                return LList.makeList(selectedTracks);
            }
        }
        
//        public static final Procedure getTrackProc = new GetTrackProc(new String[] { "get-track", "gett" });
//        @Override
//        public Procedure getGetTrack() { return getTrackProc; }
//        public static final class GetTrackProc extends MultipleNamedProcedureN {
//            public GetTrackProc(String[] names) {
//                super(names);
//            }
//
//            @Override
//            public Object applyN(Object[] args) throws Throwable {
//                // ADDED (Thu, 25 Jun 2020 23:56:05 +0900) >>>
//                return readParamGett(args);
//                // ADDED (Thu, 25 Jun 2020 23:56:05 +0900) <<<
//            }
//        }
//
//        public static final GetTrackDoc getTrackDoc = new GetTrackDoc();
//        public static final class GetTrackDoc extends PulsarProceduralDescriptiveDoc {
//            {
//                setCategory( Pulsar.DOCS_ID );
//                setNames( "get-track", "gett" );
//                setParameterDescription( "[track-spec]..." );
//                addParameter( 0, "track-spec", "any", null, true, "a subprocedure to execute by this procedure. See (help about-track-spec). " ); 
//            
//                setReturnValueDescription( "::void" );
//                setShortDescription( "||<name/>|| retrieves multiple tracks which are specified as track-spec arguments. " );
//                setLongDescription( ""
//                                    + "The tracks are stored in a linked list. "
//                                    + "See (help about-track-spec). "
//                                    + "" 
//                                    + THROWS_AN_ERROR_IF_NOT_OPEN );
//            }
//        }
//        
        
        public final Procedure getTrackPositionProc = new GetTrackPositionProc(new String[] { "get-track-position", "gettp" });
        @Override
        public Procedure getGetTrackPosition() { return getTrackPositionProc; }
        public final class GetTrackPositionProc extends MultipleNamedProcedure1 {
            public GetTrackPositionProc(String[] names) {
                super(names);
            }

            @Override
            public Object apply1( Object arg1 ) throws Throwable {
                if ( Boolean.FALSE.equals( arg1 ) ) { 
                    return Boolean.FALSE;
                } else {
                    MetroSynchronizable track = ((MetroSynchronizable)arg1);
                    double position = track.getPosition( getPulsar() );
                    return SchemeValues.toSchemeNumber( position );
                }
            }
        }

        public static final GetTrackPositionBean getTrackPositionBean = new GetTrackPositionBean();
        public static final class GetTrackPositionBean extends PulsarProceduralDescriptiveDoc {
            {
                setCategory( Pulsar.DOCS_ID );
                setNames( "get-track-position", "gettp" );
                setParameterDescription( "" );
                setReturnValueDescription( "::void" );
                setShortDescription( "||<name/>|| gets the current position of the given track." );
                setLongDescription( ""
                                    + "" 
                                 );
            }
        }

        
        
        

        public static final AboutTrackSpecDoc aboutTrackSpecDoc = new AboutTrackSpecDoc();
        public static final class AboutTrackSpecDoc extends PulsarProceduralDescriptiveDoc {
            {
                setCategory( Pulsar.DOCS_ID );
                setNames( "about-track-spec" );
                setParameterDescription( "" );
                setReturnValueDescription( "" );
                setShortDescription( "The track-spec denotes a specification of a track to retrieve. " );
                setLongDescription( ""
                                    + "Only symbol, string and procedure are valid as a track-spec.\n\n "
                                    + "When track-spec is a symbol/a string, the value is compared with the name value "
                                    + "of each track, and the track is added to the result when it equals to the value. "
                                    + "It uses the equals() method of java.lang.Object class to check the equality of the two values. \n\n"
                                    + "When track-spec is a procedure: The system enumerates all tracks in the current sequencer, "
                                    + "and call the specified procedure for each track. The procedure should have two parameters : "
                                    + "(lambda ( name tags ) ... ). If a track identified by the name and the tags is not to retrieve, "
                                    + "the procedure should return #f; otherwise the track is selected to the result. \n\n"
                                    + "" 
                                    + THROWS_AN_ERROR_IF_NOT_OPEN );
            }
        }

//        public final Procedure manipulateTrackProc = new ManipulateTrackProc( new String[] { "manipulate-track", "mant" });
//        @Override
//        public Procedure getManipulateTrack() { return manipulateTrackProc; }
//        public static final class ManipulateTrackProc extends MultipleNamedProcedureN {
//            public ManipulateTrackProc(String[] names) {
//                super(names);
//            }
//            @Override
//            public Object applyN(Object[] args) throws Throwable {
//                switch ( args.length ) {
//                    case 0:
//                        return readParamMant( "idle" );
//                    case 1:
//                        return readParamMant( SchemeValues.anyToString( args[0] ) );
//                    default :
//                        return readParamMant( 
//                            SchemeValues.anyToString( args[0] ),
//                            Arrays.copyOfRange(args, 1, args.length ) );
//                }
//            }
//        }

//        public final Procedure selectTrackProc = new SelectTrackProc( new String[] { "select-track", "selt" });
//        @Override
//        public Procedure getSelectTrack() { return selectTrackProc; }
//        public static final class SelectTrackProc extends MultipleNamedProcedureN {
//            public SelectTrackProc(String[] names) {
//                super(names);
//            }
//            @Override
//            public Object applyN(Object[] args) throws Throwable {
//                return readParamSelt(args);
//            }
//        }

        
//        public final Procedure syncTrackProc = new SyncTrackProc( new String[] { "sync-track", "synct" });
//        @Override
//        public Procedure getSyncTrack() { return syncTrackProc; }
//        public static final class SyncTrackProc extends MultipleNamedProcedureN {
//            public SyncTrackProc(String[] names) {
//                super(names);
//            }
//            @Override
//            public Object applyN(Object[] args) throws Throwable {
//                return readParamSynct(args);
//            }
//        }

//        public static final Procedure newTrackProc = new NewTrackProc( new String[] { "new-track", "newt" });
//        @Override
//        public Procedure getNewTrack() { return newTrackProc; }
//        public static final class NewTrackProc extends MultipleNamedProcedureN {
//            public NewTrackProc(String[] names) {
//                super(names);
//            }
//            @Override
//            public Object applyN(Object[] args) throws Throwable {
//                return readParamNewTrack(args);
//            }
//        }
//
//        public static final NewTrackDoc newTrackDoc = new NewTrackDoc();
//        public static final class NewTrackDoc extends PulsarProceduralDescriptiveDoc {
//            {
//                setCategory( Pulsar.DOCS_ID );
//                setNames( "new-track" , "newt" );
//                setParameterDescription( "[procedure/(list notation)]..." );
//                addParameter( 0, "notations", "procedure/(list notation)", null, true, "The contents of the track. " );
//                setReturnValueDescription( "::MetroTrack" );
//                setShortDescription( "<name/> creates a new track." );
//                setLongDescription( ""
//                                    + "A track is a basic unit of music in Pulsar music sequencer. "
//                                    + "A track contains a procedure to create a notation list. "
//                                    + "When a user added a track to the sequencer, "
//                                    + "the sequencer asks what to play next to the track. "
//                                    + "The sequencer plays it and asks to the track again when it finished to play the notation list. "
//                                    + "The length of a notation list which a track creates is usually one measure; "
//                                    + "but it can be any length. "
//                                    + "The sequencer can have multiple tracks. There is no limit on maximum number of tracks. "
//                                    + "It is necessary to add the track which is created by <name/> procedure to the "
//                                    + "sequencer by (put-track) procedure. See (help put-track) for further information. "
//                                    + "" 
//                                    + THROWS_AN_ERROR_IF_NOT_OPEN );
//            }
//        }


        public static final AboutNotationDoc aboutNotationDoc = new AboutNotationDoc();
        public static final class AboutNotationDoc extends PulsarProceduralDescriptiveDoc {
            {
                setCategory( Pulsar.DOCS_ID );
                setNames( "about-notation" );
                setParameterDescription( "" );
                setReturnValueDescription( "::void" );
                setShortDescription( "A notation is a MIDI data which Pulsar music sequencer can play. " );
                setLongDescription( ""
                                    + "In Pulsar, a notation is made of a Scheme association list. There are several types of a notation "
                                    + "such as notes, rests, MIDI control changes and others. "
                                    + "The contents of a notation depend on its type; "
                                    + "for example, if a notation is a note data, "
                                    + "the notation object have four properties : velocity, length, position and pitch. "
                                    + "" // TODO 
                                    + THROWS_AN_ERROR_IF_NOT_OPEN );
            }
        }

        public static final AboutIntroDoc aboutIntroDoc = new AboutIntroDoc();
        public static final class AboutIntroDoc extends PulsarProceduralDescriptiveDoc {
            {
                setCategory( Pulsar.DOCS_ID );
                setNames( "about-intro"  );
                setParameterDescription( "" );
                setReturnValueDescription( "" );
                setShortDescription( "Welcome to Pulsar music sequencer!" );
                setLongDescription( ""
                                    + "Pulsar music sequencer is a music sequencer which collaboratively works with "
                                    + "a powerful computer language Lisp Scheme. "
                                    + "And this frame itself is a powerful Lisp Scheme editor which is called Kawapad. "
                                    + "In Lisp, all commands are surrounded with a pair of parentheses. You can easily execute "
                                    + "one of those command by moving your cursor within the pair of parentheses and pressing CTRL+ENTER. \n\n"
                                    + "To show this help, execute (help about-intro). \n"
                                    + "To show all available procedures, execute (help) . \n"
                                    + "To show help of a procedure, execute (help [procedure-name] ) . \n"
                                    + "" 
                                 );
            }
        }

        public static final PulsarProceduralDescriptiveDoc trackManagementTemplateDoc = /*init*/( new TrackManagementTemplateDoc() );
        public static final class TrackManagementTemplateDoc extends PulsarProceduralDescriptiveDoc {
            {
                setVisible(false);
                setCategory( Pulsar.DOCS_ID );
                setParameterDescription( "track [sync-type] [sync-track] [sync-offset]" );
                addParameter( 0, "sync-type",   "symbol",     "", false, "one of ||immediate||, ||parallel|| and ||serial||. " );
                addParameter( 0, "sync-track",  "MetroTrack|track-spec", "", false, "a reference to MetroTrack object to synchronize with. " ); // XXX
                addParameter( 0, "sync-offset", "number",     "", false, "the offset value by real number. " ); 
                setReturnValueDescription( "" );
                setShortDescription( "%1$s the passed track on the sequencer. " );
                setLongDescription( ""
                                    + "%2$s\n\n"
                                    + "The ||track|| parameter is the reference to the track which is to play. \n\n"
                                    + "The sync-type parameter can be one of ||immediate||, ||parallel|| and ||serial||. \n\n"
                                    + "When sync-type is ||immediate||, the sequencer starts to play the track "
                                    + "as soon as possible after returning from the procedure call. "
                                    + "When sync-type is ||parallel||, the sequencer starts to play the track "
                                    + "at the same position with the track which is specified as ||sync-track|| parameter. \n\n"
                                    + "When sync-type is ||serial||, the sequencer starts to play the track right after the " 
                                    + "track which is specified in the ||sync-track|| finished to play. \n\n"
                                    + "The sync-track parameter is the reference to the track which is to synchronize with. \n\n"
                                    + "The sync-offset parameter is the time offset from the time that "
                                    + "track is supposed to start playing. "
                                    + "The number must be a real number. It denotes the offset length which unit is a measure-length. "
                                    + ""
                                 );
            }
        }


//        public static final LamuDocument removeTrackDoc = trackManagementTemplateDoc.processArguments( 
//            "removes",
//                  ""
//                + "The sequencer remove the specified track. Eventually the track stops playing. "
//                + "And it gives the user some controls on "
//                + "how it stops playing the track. "    
//            ).setNames( "remove-track", "remt" );
//
//        public final Procedure removeTrackProc = new RemoveTrackProc(new String[] { "remove-track", "remt" });
//        @Override
//        public Procedure getRemoveTrack() { return removeTrackProc; }
//        public final class RemoveTrackProc extends MultipleNamedProcedureN {
//            public RemoveTrackProc(String[] names) {
//                super(names);
//            }
//
//            // Reuse the objects for passing parameters to reduce the garbage-collector load.  
//            final Map<String, Object> namedArgs = new HashMap<>();
//            final List<Object> plainArgs = new ArrayList<>();
//            final Object[] trackManipulators = new Object[1]; 
//
//            @Override
//            public synchronized Object applyN(Object[] args) throws Throwable {
//                namedArgs.clear();
//                plainArgs.clear();
//                SchemeValues.parseArguments(args, namedArgs, plainArgs);
//                
//                MetroTrackSynchronizer trackSynchronizer = 
//                    (MetroTrackSynchronizer) namedArgs.get("stop");
//                
//                MetroTrackSelector trackSelector = readParamSelt(plainArgs.toArray(new Object[plainArgs.size()]));
//                MetroTrackManipulator trackManipulator = readParamMant( 
//                    "remt", 
//                    trackSelector, 
//                    trackSynchronizer);
//                
//                if ( AUTO_EXET ) {
//                    trackManipulators[0] = trackManipulator;
//                    Object result = executeTrackProc.applyN( trackManipulators );
//                    return result;
//                } else {
//                    return trackManipulator;
//                }
//            }
//        }
//
//        public static final LamuDocument putTrackDoc = trackManagementTemplateDoc.processArguments( 
//            "put",
//            ""
//            + "The sequencer starts to play the added track and it gives the user some controls on "
//            + "how it starts playing the track."
//        ).setNames( "put-track", "putt" );
//            
//        public final Procedure putTrackProc = new PutTrackProc(new String[] { "put-track", "putt" });
//        @Override
//        public Procedure getPutTrack() { return putTrackProc; }
//        public final class PutTrackProc extends MultipleNamedProcedureN {
//            public PutTrackProc(String[] names) {
//                super(names);
//            }
//            
//            // Reuse the objects for passing parameters to reduce the garbage-collector load.  
//            final Map<String, Object> namedArgs = new HashMap<>();
//            final List<Object> plainArgs = new ArrayList<>();
//            final Object[] trackManipulators = new Object[1]; 
//            
//            @Override
//            public synchronized Object applyN(Object[] args) throws Throwable {
//                namedArgs.clear();
//                plainArgs.clear();
//                SchemeValues.parseArguments(args, namedArgs, plainArgs);
//                
//                MetroTrackSynchronizer trackSynchronizer = 
//                    (MetroTrackSynchronizer) namedArgs.get("start");
//                
//                plainArgs.add(0, "newt");
//                MetroTrackSelector trackSelectors = readParamSelt(plainArgs.toArray(new Object[plainArgs.size()]));
//                MetroTrackManipulator trackManipulator = readParamMant( 
//                    "putt", 
//                    trackSelectors, 
//                    trackSynchronizer);
//
//                if ( AUTO_EXET ) {
//                    trackManipulators[0] = trackManipulator;
//                    Object result = executeTrackProc.applyN( trackManipulators );
//                    return result;
//                } else {
//                    return trackManipulator;
//                }
//            }
//        }
//
//        public static final LamuDocument replaceTrackDoc = trackManagementTemplateDoc.processArguments( 
//            "replace",
//            ""
//            + "The sequencer starts to play the added track and it gives the user some controls on "
//            + "how it starts playing the track."
//        ).setNames( "put-track", "putt" );
//            
//        public final Procedure replaceTrackProc = new ReplaceTrackProc(new String[] { "replace-track", "rept" });
//        @Override
//        public Procedure getReplaceTrack() { return replaceTrackProc; }
//        public final class ReplaceTrackProc extends MultipleNamedProcedureN {
//            public ReplaceTrackProc(String[] names) {
//                super(names);
//            }
//            
//            // Reuse the objects for passing parameters to reduce the garbage-collector load.  
//            final Map<String, Object> namedArgs = new HashMap<>();
//            final List<Object> plainArgs = new ArrayList<>();
//            final Object[] trackManipulators = new Object[1]; 
//            
//            @Override
//            public synchronized Object applyN(Object[] args) throws Throwable {
//                namedArgs.clear();
//                plainArgs.clear();
//                SchemeValues.parseArguments(args, namedArgs, plainArgs);
//                
//                MetroTrackSynchronizer startSynchronizer = 
//                    (MetroTrackSynchronizer) namedArgs.get("start");
//                MetroTrackSynchronizer stopSynchronizer = 
//                    (MetroTrackSynchronizer) namedArgs.get("stop");
//                
//                plainArgs.add(0, "newt");
//                MetroTrackSelector trackSelectors = readParamSelt(plainArgs.toArray(new Object[plainArgs.size()]));
//                MetroTrackManipulator trackManipulator = readParamMant( 
//                    "rept", 
//                    trackSelectors,
//                    startSynchronizer,
//                    stopSynchronizer );
//
//                if ( AUTO_EXET ) {
//                    trackManipulators[0] = trackManipulator;
//                    Object result = executeTrackProc.applyN( trackManipulators );
//                    return result;
//                } else {
//                    return trackManipulator;
//                }
//            }
//        }

        
//        public final Procedure notifyTrackChangeProc = new NotifyTrackChangeProc(new String[] { "notify-track-change", "nott" });
//        @Override
//        public Procedure getNotifyTrackChange() { return notifyTrackChangeProc; }
//        public final class NotifyTrackChangeProc extends MultipleNamedProcedure0 {
//            public NotifyTrackChangeProc(String[] names) {
//                super(names);
//            }
//
//            @Override
//            public Object apply0() throws Throwable {
//                getPulsar().notifyTrackChange("update");
//                return SchemeValues.NO_RESULT;
//            }
//        }
//
//        public static final NotifyTrackChangeDoc notifyTrackChangeDoc = new NotifyTrackChangeDoc();
//        public static final class NotifyTrackChangeDoc extends PulsarProceduralDescriptiveDoc {
//            {
//                setCategory( Pulsar.DOCS_ID );
//                setNames( "notify-track-change", "nott" );
//                setParameterDescription( "" );
//                setReturnValueDescription( "::void" );
//                setShortDescription( "notifies the sequencer that the track was added/deleted." );
//                setLongDescription( ""
//                                    + "When any tracks are added/deleted on the sequencer, the "
//                                    + "modification is not immediately reflects to the current state of "
//                                    + "the sequencer. After a series of adding/deleting tracks is performed by a user,"
//                                    + "the the user is mandated to call this procedure. "
//                                    + "This procedure notifies the sequencer that "
//                                    + "some tracks. And calling this procedure guarantees the tracks added/deleted "
//                                    + "on the sequencer are properly processed immediately. " 
//                                 );
//            }
//        }

//        public final ListTracksProc listTracksProc = new ListTracksProc(new String[] { "list-tracks", "lstt" });
//        @Override
//        public Procedure getListTracks() { return listTracksProc; }
//        public final class ListTracksProc extends MultipleNamedProcedure0 {
//            public ListTracksProc(String[] names) {
//                super(names);
//            }
//
//            @Override
//            public Object apply0() throws Throwable {
//                List<MetroTrack> tempAllTracks = getPulsar().replicateAllTracks(); 
//                ArrayList<Object> list = new ArrayList<>( tempAllTracks.size() );
//                for ( MetroTrack track :  tempAllTracks ) {
//                    list.add( track );
//                }
//                Collections.reverse(list);
//                
//                return Pair.makeList(list);
//       
//            }
//        }
//
//        public static final ListTracksDoc listTracksDoc = new ListTracksDoc();
//        public static final class ListTracksDoc extends PulsarProceduralDescriptiveDoc {
//            {
//                setCategory( Pulsar.DOCS_ID );
//                setNames( "list-tracks", "lstt" );
//                setParameterDescription( "" );
//                setReturnValueDescription( "::(list track ...)" );
//                setShortDescription( "||<name/>|| retrieves all tracks on the current sequencer. " );
//                setLongDescription( ""
//                                    + "The order of the tracks in the result of this procedure follows the first-in-last-out manner. "
//                                    + "That is, (car (<name/>)) always returns the last added track. "
//                                    + "" 
//                                 );
//            }
//        }

//        obsolete
//        public final MultipleNamedProcedure0 clearTracksProc = new ClearTracksProc(new String[] { "clear-tracks", "clet" });
//        @Override
//        public Procedure getClearTracks() { return clearTracksProc; }
//        public final class ClearTracksProc extends MultipleNamedProcedure0 {
//            public ClearTracksProc(String[] names) {
//                super(names);
//            }
//
//            @Override
//            public Object apply0() throws Throwable {
//                getPulsar().clearTracks();
//                return SchemeValues.NO_RESULT;
//            }
//        }
//
//        public static final ClearTracksDoc clearTracksDoc = new ClearTracksDoc();
//        public static final class ClearTracksDoc extends PulsarProceduralDescriptiveDoc {
//            {
//                setCategory( Pulsar.DOCS_ID );
//                setNames( "clear-tracks", "clet" );
//                setParameterDescription( "" );
//                setReturnValueDescription( "::void" );
//                setShortDescription( "||<name/>|| removes all tracks on the current sequencer immediately. " );
//                setLongDescription( ""
//                                    + "" 
//                                 );
//            }
//        }

//        obsolete
//        public final Procedure getMainTrackProc = new GetMainTrackProc(new String[] { "get-main-track", "getmt" });
//        @Override
//        public Procedure getGetMainTrack() { return getMainTrackProc; }
//        public final class GetMainTrackProc extends MultipleNamedProcedure0 {
//            public GetMainTrackProc(String[] names) {
//                super(names);
//            }
//
//            @Override
//            public Object apply0() throws Throwable {
//                return SchemeValues.javaNullCheck( getPulsar().getMainTrack() );
//            }
//        }
//
//        public static final GetMainTrackDoc getMainTrackDoc = new GetMainTrackDoc();
//        public static final class GetMainTrackDoc extends PulsarProceduralDescriptiveDoc {
//            {
//                setCategory( Pulsar.DOCS_ID );
//                setNames( "get-main-track", "getmt" );
//                setParameterDescription( "" );
//                setReturnValueDescription( "::void" );
//                setShortDescription( "||<name/>|| retrieves the reference to the current main track." );
//                setLongDescription( ""
//                                  + "" 
//                                 );
//            }
//        }
        
        public void initScheme( Environment env ) {
            SchemeValues.defineLambda( env, currentPulsarProc );
            SchemeValues.defineLambda( env, isCurrentPulsarPresentProc );
            SchemeValues.defineLambda( env, isOpenProc );
            SchemeValues.defineLambda( env, openProc );
            SchemeValues.defineLambda( env, closeProc );
            SchemeValues.defineLambda( env, getClientNameProc );
            SchemeValues.defineLambda( env, openOutputProc );
            SchemeValues.defineLambda( env, openInputProc );
            SchemeValues.defineLambda( env, closeOutputProc );
            SchemeValues.defineLambda( env, closeInputProc );
            SchemeValues.defineLambda( env, listOutputProc );
            SchemeValues.defineLambda( env, listInputProc );
            SchemeValues.defineLambda( env, connectProc );
            SchemeValues.defineLambda( env, disconnectProc );
            SchemeValues.defineLambda( env, listAllOutputProc );
            SchemeValues.defineLambda( env, listAllInputProc );
            SchemeValues.defineLambda( env, setMainProc );
            SchemeValues.defineLambda( env, getMainProc );
            SchemeValues.defineLambda( env, setPlayingProc );
            SchemeValues.defineLambda( env, isPlayingProc );
            SchemeValues.defineLambda( env, playProc );
            SchemeValues.defineLambda( env, stopProc );
            SchemeValues.defineLambda( env, quitProc );
            SchemeValues.defineLambda( env, tapTempoProc );
            SchemeValues.defineLambda( env, getTempoProc );
            SchemeValues.defineLambda( env, setTempoProc );
            SchemeValues.defineLambda( env, getBarsPerSecondProc );
            SchemeValues.defineLambda( env, rewindProc );
            SchemeValues.defineLambda( env, executeTrackProc );
//            SchemeValues.defineLambda( env, getTrackProc );
            SchemeValues.defineLambda( env, getTrackPositionProc );
//            SchemeValues.defineLambda( env, manipulateTrackProc );
//            SchemeValues.defineLambda( env, selectTrackProc );
//            SchemeValues.defineLambda( env, syncTrackProc );
//            SchemeValues.defineLambda( env, newTrackProc );
//            SchemeValues.defineLambda( env, newRecordingTrackProc );
//            SchemeValues.defineLambda( env, removeTrackProc );
//            SchemeValues.defineLambda( env, putTrackProc );
//            SchemeValues.defineLambda( env, notifyTrackChangeProc );
//            SchemeValues.defineLambda( env, listTracksProc );
//            SchemeValues.defineLambda( env, clearTracksProc );
//            SchemeValues.defineLambda( env, getMainTrackProc );
            SchemeValues.defineVar( env, getPulsar(), Arrays.asList( "pu" ) );
        }
        
        public static class PulsarLibStaticValueImplementation extends PulsarLibImplementation {
            private final Pulsar pulsar = new Pulsar();
            @Override
            protected Pulsar getPulsar() {
                return pulsar;
            } 
        }
    }
}
