package pulsar;


import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.logging.Level;
import java.util.stream.Collectors;

import gnu.lists.EmptyList;
import gnu.lists.IString;
import gnu.lists.LList;
import gnu.lists.Pair;
import gnu.mapping.Environment;
import gnu.mapping.Procedure;
import gnu.mapping.Symbol;
import gnu.math.IntNum;
import lamu.lib.Invokable;
import lamu.lib.helps.LamuDocument;
import lamu.lib.kawautils.SchemeInvokable;
import lamu.lib.kawautils.SchemeValues;
import lamu.lib.kawautils.procedures.MultipleNamedProcedure0;
import lamu.lib.kawautils.procedures.MultipleNamedProcedure1;
import lamu.lib.kawautils.procedures.MultipleNamedProcedure2;
import lamu.lib.kawautils.procedures.MultipleNamedProcedureN;
import lamu.lib.log.Logger;
import metro.MetroPort;
import metro.MetroSynchronizedTrack;
import metro.MetroSyncType;
import metro.MetroTrack;
import metro.MetroTradTrackSynchronizer;
import metro.MetroVoidTrack;

public interface PulsarLib {
    Procedure getGetCurrentPulsar();
    Procedure getIsCurrentPulsarPresent();
    Procedure getIsOpen();
    Procedure getOpen();
    Procedure getClose();
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
    Procedure getSimultaneous();
    Procedure getGetTrack();
    Procedure getGetTrackPosition();
    Procedure getNewTrack();
    Procedure getNewRecordingTrack();
    Procedure getRemoveTrack();
    Procedure getPutTrack();
    Procedure getNotifyTrackChange();
    Procedure getListTracks();
    Procedure getClearTracks();
    Procedure getGetMainTrack();
    public static abstract interface PulsarLibDelegator extends PulsarLib {
        abstract PulsarLib getPulsarLibImplementation();
        @Override
        default Procedure getGetMainTrack() {
            return getPulsarLibImplementation().getGetMainTrack();
        }
        @Override
        public default Procedure getGetCurrentPulsar() {
            return getPulsarLibImplementation().getGetCurrentPulsar();
        }
        @Override
        public default Procedure getIsCurrentPulsarPresent() {
            return getPulsarLibImplementation().getIsCurrentPulsarPresent();
        }

        public default Procedure getClearTracks() {
            return getPulsarLibImplementation().getClearTracks();
        }

        public default Procedure getListTracks() {
            return getPulsarLibImplementation().getListTracks();
        }

        public default Procedure getNotifyTrackChange() {
            return getPulsarLibImplementation().getNotifyTrackChange();
        }

        public default Procedure getPutTrack() {
            return getPulsarLibImplementation().getPutTrack();
        }

        public default Procedure getRemoveTrack() {
            return getPulsarLibImplementation().getRemoveTrack();
        }

        public default Procedure getNewRecordingTrack() {
            return getPulsarLibImplementation().getNewRecordingTrack();
        }

        public default Procedure getNewTrack() {
            return getPulsarLibImplementation().getNewTrack();
        }

        public default Procedure getGetTrack() {
            return getPulsarLibImplementation().getGetTrack();
        }
        public default Procedure getGetTrackPosition() {
            return getPulsarLibImplementation().getGetTrackPosition();
        }

        public default Procedure getSimultaneous() {
            return getPulsarLibImplementation().getSimultaneous();
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

        protected abstract Pulsar getPulsar();
        private static final class TagSearchIsProcedure extends MultipleNamedProcedure2 {
            private final Object value;
            TagSearchIsProcedure(Object value) {
                this.value = value;
            }
            @Override
            public Object apply2( Object arg1, Object arg2 ) throws Throwable {
                return value.equals( arg1 );
            }
        }

        final static class TrackProcedure extends MultipleNamedProcedure0 {
            final LList pair;
            TrackProcedure( LList pair ) {
                this.pair = pair;
            }

            @Override
            public Object apply0() throws Throwable {
                return pair;
            }
        }
        
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
         * Wrap another invokable object in order to filter the undesirable arguments for
         * readParamTrackSearcher().
         * 
         * The tags property of MetroTrack accepts descendants of Collection class. In
         * the general use case of MetroTrack in Pulsar, it presumes that tags are LList
         * objects. The list object in ||tags|| property is passed to clients directly.
         * But since the MetroTrack accepts all types of Collection class descendants,
         * MetroTrack forces Pulsar to support tags objects which is other than LList.
         * 
         * Avoid unnecessary duplication of passed lists, we check the type of each list.
         * And let is pass through when it is an LList list, and convert it to LList when
         * it is a general Collection list. (Thu, 22 Aug 2019 12:18:27 +0900) 
         * 
         * @param i
         *     
         * @return
         */
        static Invokable readParamSearchTrackFilter( Invokable i ) {
            return new Invokable() {
                @Override
                public Object invoke(Object... args) {
                    if ( 0 < args.length ) {
                        args[1] = filterArg( args[1] );
                    }
                    return i.invoke( args );
                }
                Object filterArg(Object arg1) {
                    if ( arg1 == null ) {
                        return EmptyList.emptyList;
                    } else if ( arg1 instanceof LList ) {
                        return arg1;
                    } else if ( arg1 instanceof Collection ) {
                        if (((Collection)arg1).isEmpty()) {
                            return EmptyList.emptyList;
                        } else {
                            return Pair.makeList( Arrays.asList(((Collection)arg1).toArray()));
                        }
                    } else {
                        return arg1;
                    }
                }
            };
        }
        
        /**
         * XXX 
         * 
         * @param object
         * @return
         */
        static Procedure readParamTrackSearcher( Object object ) { 
            object = SchemeValues.schemeNullCheck( object );
            // TAG SEARCH
            if ( object instanceof Procedure ) {
                return (Procedure) object;
            } else if ( object instanceof Symbol || object instanceof IString ) {
                return new TagSearchIsProcedure(object);
            } else {
                throw new IllegalArgumentException( "unsupported type of the argument (" + object + ")" );
            }
        }
        
        
        /**
         * This method used be `searchTrackCombo()`.
         * This was renamed at (Wed, 06 Nov 2019 07:38:28 +0900). 
         * @param arg
         * @return
         */
        List<MetroTrack> readParamSearchTrack(Object arg) {
            return 
                    getPulsar().getTracks(
                        readParamSearchTrackFilter(
                            SchemeInvokable.create( readParamTrackSearcher( arg ) )));
        }

        // TODO
//        List<MetroTrack> readParamCreateTrack( Object object ) {
//            if ( object instanceof MetroTrack ) {
//                return Arrays.<MetroTrack>asList((MetroTrack)object);
//            } else if ( object instanceof Procedure ) {
//                return Arrays.asList( PulsarTrack.createTrack( null, null, (Procedure)object ));
//            } else if ( object instanceof LList ) {
//                if ( ((LList)object).isEmpty() ) {
//                    return Collections.emptyList();
//                } else {
//                    if ( object instanceof Pair ) {
//                        if (((Pair)object).getCar() instanceof MetroTrack) {
//                            return (List<MetroTrack>) object;
//                        } else if ( NoteListParser.isNotationList(object) ) {
//                            return Arrays.asList( PulsarTrack.createTrack( null, null, new TrackProcedure( (Pair) object ) ) );
//                        } else {
//                            return readParamSearchTrack( object );
//                        }
//                    } else {
//                        throw new IllegalArgumentException("unknown type of argument (" + object + ")" ) ;
//                    }
//                } 
//            } else {
//                return readParamSearchTrack( object );
//            }
//        }
        List<MetroTrack> readParamCreateTrack( Object object ) {
            if ( object instanceof MetroTrack ) {
                return Arrays.<MetroTrack>asList((MetroTrack)object);
            } else if ( object instanceof LList ) {
                if ( ((LList)object).isEmpty() ) {
                    return Collections.emptyList();
                } else {
                    List<MetroTrack> list = new ArrayList<>();
                    for ( Object o : ((LList)object)) {
                        if ( o instanceof MetroTrack ) {
                            list.add((MetroTrack) o);
                        } else {
                            list.addAll( readParamSearchTrack( object ) );
                        }
                    }
                    return list;
                } 
            } else {
                return readParamSearchTrack( object );
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
        static Procedure readParamProcedure(Object arg) {
            if ( arg  == null ) {
                return null;
            } else if ( arg instanceof Procedure ) {
                return (Procedure) arg;
            } else if ( arg  instanceof Pair ) {
                return new TrackProcedure((Pair)arg);
            } else if ( arg  instanceof Number ) {
                return new TrackProcedure( createRestBar(((Number)arg).intValue() ) );
            } else {
                throw new IllegalArgumentException( "unsupported type of the argument (" + arg + ")" );
            }
        }

        private static LList createRestBar(int intValue) {
            return 
                    LList.makeList( Arrays.asList( 
                        LList.makeList( Arrays.asList(
                            Pair.make( Symbol.valueOf( "type" ),  Symbol.valueOf( "len" ) ),
                            Pair.make( Symbol.valueOf( "val" ),   IntNum.valueOf( intValue ))))));
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
        
        
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        //
        //
        //
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        

        private static final String THROWS_AN_ERROR_IF_NOT_OPEN = 
            "In case the current sequencer system has not established any connection to the JACK, " + 
                "it throws an exception. ";

        private static final String ALTERS_THE_CURRENT_STATE =
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

        public final Procedure simultaneousProc = new SimultaneousProc(new String[] { "simultaneous", "simul" });
        @Override
        public Procedure getSimultaneous() { return simultaneousProc; }
        public final class SimultaneousProc extends MultipleNamedProcedureN {
            public SimultaneousProc(String[] names) {
                super(names);
            }

            @Override
            public Object applyN(Object[] args) throws Throwable {
                Pulsar pulsar = getPulsar();
                synchronized ( pulsar.getMetroLock() ) {
                    try {
                        pulsar.enterTrackChangeBlock();
                        for ( int i=0; i<args.length; i++ ) {
                            Object arg = args[i];
                            if ( arg instanceof Procedure ) {
                                ((Procedure)arg).apply0();
                            } else {
                                logWarn( "The value in args[" + i + "] was not a procedure. Ignored. "  );
                            }
                        }
                    } finally {
                        pulsar.leaveTrackChangeBlock();
                        pulsar.notifyTrackChange("update");
                    }
                }
                return SchemeValues.NO_RESULT;
            }
        }

        public static final SimultaneousDoc simultaneousDoc = new SimultaneousDoc();
        public static final class SimultaneousDoc extends PulsarProceduralDescriptiveDoc {
            {
                setCategory( Pulsar.DOCS_ID );
                setNames( "simultaneous", "simul" );
                setParameterDescription( "[procedure]..." );
                addParameter( 0, "subproc", "procedure", null, true, "a subprocedure to execute by this procedure. " ); 
                setReturnValueDescription( "::void" );
                setShortDescription( "executes passed the procedures \"simultaneously\". " );
                setLongDescription( ""
                                    + "This procedure is designed to add multiple tracks to the sequencer and and let the tracks start "
                                    + "to play simultaneously. While the interpreter is executing this procedure, the thread "
                                    + "that is processing tracks in order to generate the music data is blocked. "
                                    + "Therefore, it is guaranteed that the sequencer starts to process the added tracks simultaneously. "
                                    + "" 
                                    + THROWS_AN_ERROR_IF_NOT_OPEN );
            }
        }

        public final Procedure getTrackProc = new GetTrackProc(new String[] { "get-track", "gett" });
        @Override
        public Procedure getGetTrack() { return getTrackProc; }
        public final class GetTrackProc extends MultipleNamedProcedureN {
            public GetTrackProc(String[] names) {
                super(names);
            }

            @Override
            public Object applyN(Object[] args) throws Throwable {
                ArrayList<MetroTrack> t = new ArrayList<>();
                for ( int i=0; i<args.length; i++ ) {
                    Object arg = args[i];
                    t.addAll( readParamSearchTrack( arg ) );
                }
                return Pair.makeList( t );
            }
        }

        public static final GetTrackDoc getTrackDoc = new GetTrackDoc();
        public static final class GetTrackDoc extends PulsarProceduralDescriptiveDoc {
            {
                setCategory( Pulsar.DOCS_ID );
                setNames( "get-track", "gett" );
                setParameterDescription( "[track-spec]..." );
                addParameter( 0, "track-spec", "any", null, true, "a subprocedure to execute by this procedure. See (help about-track-spec). " ); 
            
                setReturnValueDescription( "::void" );
                setShortDescription( "||<name/>|| retrieves multiple tracks which are specified as track-spec arguments. " );
                setLongDescription( ""
                                    + "The tracks are stored in a linked list. "
                                    + "See (help about-track-spec). "
                                    + "" 
                                    + THROWS_AN_ERROR_IF_NOT_OPEN );
            }
        }
        
        
        
        
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
                    MetroSynchronizedTrack track = ((MetroSynchronizedTrack)arg1);
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

        public final Procedure newTrackProc = new NewTrackProc( new String[] { "new-track", "newt" });
        @Override
        public Procedure getNewTrack() { return newTrackProc; }
        public final class NewTrackProc extends MultipleNamedProcedureN {
            public NewTrackProc(String[] names) {
                super(names);
            }

            @Override
            public Object applyN(Object[] args) throws Throwable {
                Object name         = null;
                List<Object> tags   = null;
                Procedure procedure=null;
                MetroTrack track=null;
                
                MetroSyncType syncType;
                List<MetroTrack> syncTrackList;
                double syncOffset;
                

                switch ( args.length  ){
                    case 0 : 
                        track = new MetroVoidTrack(null, null);
                    case 1 :
                        name          = null;
                        tags          = null;
                        procedure     = readParamProcedure(args[0]);
                        syncType      = MetroSyncType.IMMEDIATE;
                        syncTrackList = Collections.EMPTY_LIST; 
                        syncOffset    = 0.0d;
                        break;
                    case 2 : {
                        List<Object> lst = readParamTrackName( args[0] );
                        name = lst.remove(0);
                        tags = lst;
                        procedure     = readParamProcedure(args[1]);
                        syncType      = MetroSyncType.IMMEDIATE;
                        syncTrackList = Collections.EMPTY_LIST; 
                        syncOffset    = 0.0d;
                        break;
                    }   
                    case 3 : {
                        List<Object> lst = readParamTrackName( args[0] );
                        name = lst.remove(0);
                        tags = lst;
                        procedure     = readParamProcedure(args[1]);
                        syncType      = readParamSyncType( args[2] );
                        syncTrackList = Collections.EMPTY_LIST; 
                        syncOffset    = 0.0d;
                        break;
                    }
                    case 4 : {
                        List<Object> lst = readParamTrackName( args[0] );
                        name = lst.remove(0);
                        tags = lst;
                        procedure     = readParamProcedure(args[1]);
                        syncType      = readParamSyncType( args[2] );
                        syncTrackList = readParamCreateTrack( args[3] );
                        syncOffset    = 0.0d;
                        break;
                    }
                    case 5 : {
                        List<Object> lst = readParamTrackName( args[0] );
                        name = lst.remove(0);
                        tags = lst;
                        procedure     = readParamProcedure(args[1]);
                        syncType      = readParamSyncType( args[2] );
                        syncTrackList = readParamCreateTrack( args[3] );
                        syncOffset    = readParamSyncOffset( args[4] );
                        break;
                    }
                    default :
                        throw new IllegalArgumentException();
                }

                MetroTrack syncTrack = syncTrackList.isEmpty() ? null : syncTrackList.get(0);

                
                if ( track == null ) {
                    if ( syncTrack!=null && !( syncTrack instanceof MetroSynchronizedTrack)) {
                        throw new IllegalArgumentException( "syncTrack must be a MetroSyncTrack object" );
                    }
                    track = PulsarTrack.createTrack( name, tags, syncType, syncTrack, syncOffset, procedure);
                }
                
                return track;
            }
        }

        public static final NewTrackDoc newTrackDoc = new NewTrackDoc();
        public static final class NewTrackDoc extends PulsarProceduralDescriptiveDoc {
            {
                setCategory( Pulsar.DOCS_ID );
                setNames( "new-track" , "newt" );
                setParameterDescription( "[procedure/(list notation)]..." );
                addParameter( 0, "notations", "procedure/(list notation)", null, true, "The contents of the track. " );
                setReturnValueDescription( "::MetroTrack" );
                setShortDescription( "<name/> creates a new track." );
                setLongDescription( ""
                                    + "A track is a basic unit of music in Pulsar music sequencer. "
                                    + "A track contains a procedure to create a notation list. "
                                    + "When a user added a track to the sequencer, "
                                    + "the sequencer asks what to play next to the track. "
                                    + "The sequencer plays it and asks to the track again when it finished to play the notation list. "
                                    + "The length of a notation list which a track creates is usually one measure; "
                                    + "but it can be any length. "
                                    + "The sequencer can have multiple tracks. There is no limit on maximum number of tracks. "
                                    + "It is necessary to add the track which is created by <name/> procedure to the "
                                    + "sequencer by (put-track) procedure. See (help put-track) for further information. "
                                    + "" 
                                    + THROWS_AN_ERROR_IF_NOT_OPEN );
            }
        }

        public final Procedure newRecordingTrackProc = new NewRecordTrackProc( new String[] { "new-recording-track", "rect" });
        @Override
        public Procedure getNewRecordingTrack() { return newRecordingTrackProc; }
        public final class NewRecordTrackProc extends MultipleNamedProcedureN {
            public NewRecordTrackProc(String[] names) {
                super(names);
            }

            @Override
            public Object applyN(Object[] args) throws Throwable {
                Pulsar current = getPulsar();
                Object name;
                List<Object> tags;
                List<MetroPort> inputPorts;
                List<MetroPort> outputPorts;
                double recordLength;
                boolean looper;
       
                switch ( args.length  ){
                    case 0 :
                    case 1 : 
                    case 2 : 
                        throw new IllegalArgumentException();
                    case 3 : 
                    case 4 : 
                    case 5 : 
                    {
                        List<Object> lst = readParamTrackName( args[0] );
                        name = lst.remove(0);
                        tags = null;
                        {
                            List<MetroPort> ports = readParamPort( args[1], current.getInputPorts() );
                            if ( ports.size() == 0 )
                                throw new IllegalArgumentException("could not find input port " + args[1] );
                            inputPorts = ports; 
                        }
                        {
                            List<MetroPort> ports = readParamPort( args[2], current.getOutputPorts() );
                            if ( ports.size() == 0 )
                                throw new IllegalArgumentException("could not find output port " + args[2] );
                            outputPorts = ports; 
                        }
                        
                        if ( 3< args.length ) {
                            recordLength = SchemeValues.toDouble( args[3] );
                        } else {
                            recordLength = -1;
                        }
                        if ( 4< args.length ) {
                            looper = SchemeValues.toBoolean( args[4] );
                        } else {
                            looper = true;
                        }
                        break;
                    }
                    
                    default :
                        throw new IllegalArgumentException();
                }
                return PulsarTrack.createRecordingTrack( name, tags, inputPorts, outputPorts, recordLength, looper );
            }
        }

        public static final NewRecordingTrackDoc newRecordingTrackDoc = new NewRecordingTrackDoc();
        public static final class NewRecordingTrackDoc extends PulsarProceduralDescriptiveDoc {
            {
                setCategory( Pulsar.DOCS_ID );
                setNames( "new-recording-track" , "rect" );
                setParameterDescription( "[procedure/(list notation)]..." );
                addParameter( 0, "notations", "procedure/(list notation)", null, true, "The contents of the track. " );
                setReturnValueDescription( "::MetroTrack" );
                setShortDescription( "<name/> creates a new track." );
                setLongDescription( ""
                                    + "" 
                                    + THROWS_AN_ERROR_IF_NOT_OPEN );
            }
        }

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

        public abstract class TrackManagementProc extends MultipleNamedProcedureN  {
            TrackManagementProc( String ... names ) {
                super(names);
            }
            abstract void procTrack( List<MetroTrack> trackList, MetroSyncType syncType, MetroTrack syncTrack, double syncOffset );
            
            @Override
            public Object applyN(Object[] args) throws Throwable {
                List<MetroTrack> trackList;
                MetroSyncType syncType;
                List<MetroTrack> syncTrackList;
                double syncOffset;
                switch ( args.length ) {
                    case 0 :
                        throw new IllegalArgumentException();
                    case 1 :
                        trackList     = readParamCreateTrack( args[0] );
                        syncType      = MetroSyncType.IMMEDIATE;
                        syncTrackList = Collections.EMPTY_LIST;
                        syncOffset    = 0.0d;
                        break;
                    case 2 :
                        trackList     = readParamCreateTrack( args[0] );
                        syncType      = readParamSyncType( args[1] );
                        syncTrackList = Collections.EMPTY_LIST;
                        syncOffset    = 0.0d;
                        break;
                    case 3 :
                        trackList     = readParamCreateTrack( args[0] );
                        syncType      = readParamSyncType( args[1] );
                        syncTrackList = readParamCreateTrack( args[2] );
                        syncOffset    = 0.0d;
                        break;
                    case 4 :
                        trackList     = readParamCreateTrack( args[0] );
                        syncType      = readParamSyncType( args[1] );
                        syncTrackList = readParamCreateTrack( args[2] );
                        syncOffset    = readParamSyncOffset( args[3] );
                        break;
                    default :
                        throw new IllegalArgumentException();
                }
                
                MetroTrack syncTrack;
                if ( syncTrackList.size() == 0 ) {
                    syncTrack = null;
                } else {
                    syncTrack = syncTrackList.get(0);
                }
                procTrack( trackList, syncType, syncTrack, syncOffset );
                
                
                /*
                 * (Wed, 09 Oct 2019 17:35:14 +0900) RETURNING_LISTS_CAUSE_ERROR
                 * 
                 * Returning a list which is created by Java DOES cause 
                 * a problem in Scheme code. It will miraculously be merged with the
                 * values which are returned by following code. The merged mysterious value
                 * cannot be checked its type. The exact reason why this happens is unknown.
                 * Googling this gives me no result.
                 *  
                 */
//                    return Pair.makeList( trackList );
////                  return Values.empty;
                
                // I want it to get back (Sun, 03 Nov 2019 04:56:43 +0900)
//                    return Values.empty;
                return LList.makeList( trackList );
            }
        }

        public static final LamuDocument removeTrackDoc = trackManagementTemplateDoc.processArguments( 
            "removes",
                  ""
                + "The sequencer remove the specified track. Eventually the track stops playing. "
                + "And it gives the user some controls on "
                + "how it stops playing the track. "    
            ).setNames( "remove-track", "remt" );

        public final Procedure removeTrackProc = new RemoveTrackProc(new String[] { "remove-track", "remt" });
        @Override
        public Procedure getRemoveTrack() { return removeTrackProc; }
        public final class RemoveTrackProc extends TrackManagementProc {
            public RemoveTrackProc(String[] names) {
                super(names);
            }

            @Override
            void procTrack( List<MetroTrack> trackList, MetroSyncType syncType, MetroTrack syncTrack, double syncOffset ) {
                getPulsar().removeTrack(trackList, MetroTradTrackSynchronizer.create( syncType, syncTrack, syncOffset ));
            }
        }

        public static final LamuDocument putTrackDoc = trackManagementTemplateDoc.processArguments( 
            "put",
            ""
            + "The sequencer starts to play the added track and it gives the user some controls on "
            + "how it starts playing the track."    
        ).setNames( "put-track", "putt" );
            
        public final Procedure putTrackProc = new PutTrackProc(new String[] { "put-track", "putt" });
        @Override
        public Procedure getPutTrack() { return putTrackProc; }
        public final class PutTrackProc extends TrackManagementProc {
            public PutTrackProc(String[] names) {
                super(names);
            }

            @Override
            void procTrack( List<MetroTrack> trackList, MetroSyncType syncType, MetroTrack syncTrack, double syncOffset ) {
                getPulsar().putTrack(trackList, syncType, syncTrack, syncOffset);
            }
        }

        
        public final Procedure notifyTrackChangeProc = new NotifyTrackChangeProc(new String[] { "notify-track-change", "nott" });
        @Override
        public Procedure getNotifyTrackChange() { return notifyTrackChangeProc; }
        public final class NotifyTrackChangeProc extends MultipleNamedProcedure0 {
            public NotifyTrackChangeProc(String[] names) {
                super(names);
            }

            @Override
            public Object apply0() throws Throwable {
                getPulsar().notifyTrackChange("update");
                return SchemeValues.NO_RESULT;
            }
        }

        public static final NotifyTrackChangeDoc notifyTrackChangeDoc = new NotifyTrackChangeDoc();
        public static final class NotifyTrackChangeDoc extends PulsarProceduralDescriptiveDoc {
            {
                setCategory( Pulsar.DOCS_ID );
                setNames( "notify-track-change", "nott" );
                setParameterDescription( "" );
                setReturnValueDescription( "::void" );
                setShortDescription( "notifies the sequencer that the track was added/deleted." );
                setLongDescription( ""
                                    + "When any tracks are added/deleted on the sequencer, the "
                                    + "modification is not immediately reflects to the current state of "
                                    + "the sequencer. After a series of adding/deleting tracks is performed by a user,"
                                    + "the the user is mandated to call this procedure. "
                                    + "This procedure notifies the sequencer that "
                                    + "some tracks. And calling this procedure guarantees the tracks added/deleted "
                                    + "on the sequencer are properly processed immediately. " 
                                 );
            }
        }

        public final ListTracksProc listTracksProc = new ListTracksProc(new String[] { "list-tracks", "lstt" });
        @Override
        public Procedure getListTracks() { return listTracksProc; }
        public final class ListTracksProc extends MultipleNamedProcedure0 {
            public ListTracksProc(String[] names) {
                super(names);
            }

            @Override
            public Object apply0() throws Throwable {
                List<MetroTrack> tempAllTracks = getPulsar().replicateAllTracks(); 
                ArrayList<Object> list = new ArrayList<>( tempAllTracks.size() );
                for ( MetroTrack track :  tempAllTracks ) {
                    list.add( track );
                }
                Collections.reverse(list);
                
                return Pair.makeList(list);
       
            }
        }

        public static final ListTracksDoc listTracksDoc = new ListTracksDoc();
        public static final class ListTracksDoc extends PulsarProceduralDescriptiveDoc {
            {
                setCategory( Pulsar.DOCS_ID );
                setNames( "list-tracks", "lstt" );
                setParameterDescription( "" );
                setReturnValueDescription( "::(list track ...)" );
                setShortDescription( "||<name/>|| retrieves all tracks on the current sequencer. " );
                setLongDescription( ""
                                    + "The order of the tracks in the result of this procedure follows the first-in-last-out manner. "
                                    + "That is, (car (<name/>)) always returns the last added track. "
                                    + "" 
                                 );
            }
        }

        public final MultipleNamedProcedure0 clearTracksProc = new ClearTracksProc(new String[] { "clear-tracks", "clet" });
        @Override
        public Procedure getClearTracks() { return clearTracksProc; }
        public final class ClearTracksProc extends MultipleNamedProcedure0 {
            public ClearTracksProc(String[] names) {
                super(names);
            }

            @Override
            public Object apply0() throws Throwable {
                getPulsar().clearTracks();
                return SchemeValues.NO_RESULT;
            }
        }

        public static final ClearTracksDoc clearTracksDoc = new ClearTracksDoc();
        public static final class ClearTracksDoc extends PulsarProceduralDescriptiveDoc {
            {
                setCategory( Pulsar.DOCS_ID );
                setNames( "clear-tracks", "clet" );
                setParameterDescription( "" );
                setReturnValueDescription( "::void" );
                setShortDescription( "||<name/>|| removes all tracks on the current sequencer immediately. " );
                setLongDescription( ""
                                    + "" 
                                 );
            }
        }

        public final Procedure getMainTrackProc = new GetMainTrackProc(new String[] { "get-main-track", "getmt" });
        @Override
        public Procedure getGetMainTrack() { return getMainTrackProc; }
        public final class GetMainTrackProc extends MultipleNamedProcedure0 {
            public GetMainTrackProc(String[] names) {
                super(names);
            }

            @Override
            public Object apply0() throws Throwable {
                return SchemeValues.javaNullCheck( getPulsar().getMainTrack() );
            }
        }

        public static final GetMainTrackDoc getMainTrackDoc = new GetMainTrackDoc();
        public static final class GetMainTrackDoc extends PulsarProceduralDescriptiveDoc {
            {
                setCategory( Pulsar.DOCS_ID );
                setNames( "get-main-track", "getmt" );
                setParameterDescription( "" );
                setReturnValueDescription( "::void" );
                setShortDescription( "||<name/>|| retrieves the reference to the current main track." );
                setLongDescription( ""
                                  + "" 
                                 );
            }
        }
        
        public void initScheme( Environment env ) {
            SchemeValues.defineLambda( env, currentPulsarProc );
            SchemeValues.defineLambda( env, isCurrentPulsarPresentProc );
            SchemeValues.defineLambda( env, isOpenProc );
            SchemeValues.defineLambda( env, openProc );
            SchemeValues.defineLambda( env, closeProc );
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
            SchemeValues.defineLambda( env, simultaneousProc );
            SchemeValues.defineLambda( env, getTrackProc );
            SchemeValues.defineLambda( env, getTrackPositionProc );
            SchemeValues.defineLambda( env, newTrackProc );
            SchemeValues.defineLambda( env, newRecordingTrackProc );
            SchemeValues.defineLambda( env, removeTrackProc );
            SchemeValues.defineLambda( env, putTrackProc );
            SchemeValues.defineLambda( env, notifyTrackChangeProc );
            SchemeValues.defineLambda( env, listTracksProc );
            SchemeValues.defineLambda( env, clearTracksProc );
            SchemeValues.defineLambda( env, getMainTrackProc );
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
