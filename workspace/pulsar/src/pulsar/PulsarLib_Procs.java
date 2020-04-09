package pulsar;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import gnu.kawa.io.Path;
import gnu.lists.LList;
import gnu.lists.Pair;
import gnu.mapping.Environment;
import gnu.mapping.Procedure;
import gnu.mapping.Values;
import gnu.mapping.WrongArguments;
import gnu.math.DFloNum;
import kawa.Shell;
import lamu.lib.doc.LamuDocument;
import lamu.lib.scheme.InvokableSchemeProcedure;
import lamu.lib.scheme.SchemeUtils;
import lamu.lib.scheme.proc.MultipleNamedProcedure0;
import lamu.lib.scheme.proc.MultipleNamedProcedure1;
import lamu.lib.scheme.proc.MultipleNamedProcedure2;
import lamu.lib.scheme.proc.MultipleNamedProcedure3;
import lamu.lib.scheme.proc.MultipleNamedProcedureN;
import lamu.lib.secretary.Invokable;
import lamu.utils.lib.PulsarSharedTimer;
import metro.EventListenable;
import metro.MetroPort;
import metro.MetroSyncType;
import metro.MetroTrack;

public class PulsarLib_Procs {
    public static final String DOCS_ID = "pulsar-procedures";

    private static final String THROWS_AN_ERROR_IF_NOT_OPEN = 
        "In case the current sequencer system has not established any connection to the JACK, " + 
            "it throws an exception. ";

    private static final String ALTERS_THE_CURRENT_STATE =
        "This procedure alters the current sequencer system's state. ";
    
    public static abstract class PulsarProceduralDescriptiveBean extends LamuDocument {
        public PulsarProceduralDescriptiveBean() {
        }
    }

    static <Bean extends LamuDocument> Bean init( Object target, Bean bean ) {
//        if ( bean != null )
//            DescriptiveDocumentCategory.initDoc(PulsarDocuments.DOCS, target, bean);
        return bean;
    }

    public static final PulsarBean pulsarBean = new PulsarBean();
    public static final class PulsarBean extends PulsarProceduralDescriptiveBean {
        {
            setCategory( DOCS_ID );
            setNames( "pulsar" );
            setParameterDescription( "" );
            setReturnValueDescription( "" );
            setShortDescription(       "" );
            setLongDescription(        ""
                                     + "" );
        }
    }
    
    public static final PulsarProc pulsarProc = new PulsarProc(new String[] { "pulsar" });
    public static final class PulsarProc extends MultipleNamedProcedure0 {
        public PulsarProc(String[] names) {
            super(names);
        }

        @Override
        public Object apply0() throws Throwable {
            return Pulsar.getCurrent();
        }
    }
    static{ init( pulsarProc , pulsarBean ); } 

    public static LamuDocument isPulsarPresentBean;

    public static final IsPulsarPresentProc isPulsarPresentProc = new IsPulsarPresentProc(new String[] { "pulsar-present?" });
    public static final class IsPulsarPresentProc extends MultipleNamedProcedure0 {
        public IsPulsarPresentProc(String[] names) {
            super(names);
        }

        @Override
        public Object apply0() throws Throwable {
            return Pulsar.isPresent();
        }
    }
    static{ init( isPulsarPresentProc , isPulsarPresentBean ); } 

    public static final IsOpenBean isOpenBean = new IsOpenBean();
    public static final class IsOpenBean extends PulsarProceduralDescriptiveBean {
        {
            setCategory( DOCS_ID );
            setNames( "open?" );
            setParameterDescription( "" );
            setReturnValueDescription( "::boolean" );
            setShortDescription(       "||<name/>|| returns the current open state. " );
            setLongDescription(        "This procedure returns #t iff the current sequencer state is open; "
                                     + "otherwise returns #f. " );
        }
    }

    public static final IsOpenProc isOpenProc = new IsOpenProc(new String[] { "open?" });
    public static final class IsOpenProc extends MultipleNamedProcedureN {
        public IsOpenProc(String[] names) {
            super(names);
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            return Pulsar.getCurrent().isOpened();
        }
    }
    static{ init( isOpenProc , isOpenBean ); } 


    public static final OpenBean openBean = new OpenBean();
    public static final class OpenBean extends PulsarProceduralDescriptiveBean {
        {
            setCategory( DOCS_ID );
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

    public static final OpenProc openProc = new OpenProc(new String[] { "open" });
    public static final class OpenProc extends MultipleNamedProcedure1 {
        public OpenProc(String[] names) {
            super(names);
        }

        @Override
        public Object apply1(Object arg0) throws Throwable {
            Pulsar.getCurrent().open( SchemeUtils.toString( arg0 ) );
            return SchemeUtils.NO_RESULT;
        }
    }
    static{ init( openProc , openBean ); } 


    public static final CloseBean closeBean = new CloseBean();
    public static final class CloseBean extends PulsarProceduralDescriptiveBean {
        {
            setCategory( DOCS_ID );
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

    public static final CloseProc closeProc = new CloseProc(new String[] { "close" });
    public static final class CloseProc extends MultipleNamedProcedureN {
        public CloseProc(String[] names) {
            super(names);
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            Pulsar.getCurrent().close();
            return SchemeUtils.NO_RESULT;
        }
    }
    static{ init( closeProc , closeBean ); } 


    public static final PulsarProceduralDescriptiveBean openPortTemplateBean = new OpenPortTemplateBean(); 
    public static final class OpenPortTemplateBean extends PulsarProceduralDescriptiveBean {{
            setCategory( DOCS_ID );
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

    public static final LamuDocument openOutputBean = ( openPortTemplateBean.processArguments( "output" ).setNames( "open-output", "openo" ) );

    public static final Procedure openOutputProc = new OpenOutputProc(new String[] { "open-output", "openo" });
    public static final class OpenOutputProc extends MultipleNamedProcedureN {
        public OpenOutputProc(String[] names) {
            super(names);
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            Pulsar pulsar = Pulsar.getCurrent();
            ArrayList<MetroPort> list = new ArrayList<>();
            for ( Object o : args ) {
                for ( Object portName : Pulsar.readParamPortName( o ) ) {
                    list.add( pulsar.createOutputPort(portName) );
                }
            }
            Collections.reverse( list );
            return LList.makeList( list );
        }
    }
    static{ init( openOutputProc , openOutputBean ); } 

    public static final LamuDocument openInputBean =  openPortTemplateBean.processArguments( "input" ).setNames( "open-input" , "openi" );

    public static final Procedure openInputProc = new OpenInputProc(new String[] { "open-input", "openi" });
    public static final class OpenInputProc extends MultipleNamedProcedureN {
        public OpenInputProc(String[] names) {
            super(names);
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            Pulsar pulsar = Pulsar.getCurrent();
            ArrayList<MetroPort> list = new ArrayList<>();
            for ( Object o : args ) {
                for ( Object portName : Pulsar.readParamPortName( o ) ) {
                    list.add( pulsar.createInputPort(portName) );
                }
            }
            Collections.reverse( list );
            return LList.makeList( list );
        }
    }
    static{ init( openInputProc , openInputBean ); } 

    public static final ClosePortTemplateBean closePortTemplateBean =  new ClosePortTemplateBean();
    public static final class ClosePortTemplateBean extends PulsarProceduralDescriptiveBean {{
        setCategory( DOCS_ID );
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

    public static final LamuDocument closeOutputBean =  closePortTemplateBean.processArguments( "output" ).setNames( "close-output" , "closeo" );

    public static final Procedure closeOutputProc = new CloseOutputProc(new String[] { "close-output", "closeo" });
    public static final class CloseOutputProc extends MultipleNamedProcedureN {
        public CloseOutputProc(String[] names) {
            super(names);
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            Pulsar pulsar = Pulsar.getCurrent();
            for ( Object o : args ) {
                for ( MetroPort p : pulsar.readParamPort( o, pulsar.getOutputPorts() ) ) {
                    pulsar.destroyOutputPort( p );
                }
            }
            return SchemeUtils.NO_RESULT;
        }
    }
    static{ init( closeOutputProc , closeOutputBean ); } 

    public static final LamuDocument closeInputBean =  closePortTemplateBean.processArguments( "input" ).setNames( "close-input", "closei" );

    public static final Procedure closeInputProc = new CloseInputProc(new String[] { "close-input", "closei" });
    public static final class CloseInputProc extends MultipleNamedProcedureN {
        public CloseInputProc(String[] names) {
            super(names);
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            Pulsar pulsar = Pulsar.getCurrent();
            for ( Object o : args ) {
                for ( MetroPort p : pulsar.readParamPort( o, pulsar.getInputPorts() ) ) {
                    pulsar.destroyInputPort( p );
                }
            }
            return SchemeUtils.NO_RESULT;
        }
    }
    static{ init( closeInputProc , closeInputBean ); } 

    public static final ListPortsTemplateBean listPortsTemplateBean =  new ListPortsTemplateBean();
    public static final class ListPortsTemplateBean extends PulsarProceduralDescriptiveBean {{
        setCategory( DOCS_ID );
        setParameterDescription( "" );
        setReturnValueDescription( "::(list MetroPort ...)" );
        setShortDescription( "returns a list which contains all %s ports on the current JACK connection. " );
        setLongDescription( ""
                            + "Each element on the list is a reference to a MetroPort object. "
                            + "The values in the list are sorted from newest to oldest. "
                            + THROWS_AN_ERROR_IF_NOT_OPEN
                            );
    }}

    public static final LamuDocument listOutputBean =  listPortsTemplateBean.processArguments( "output" ).setNames("list-output" , "lso" );

    public static final Procedure listOutputProc = new ListOutputProc(new String[] { "list-output", "lso" });
    public static final class ListOutputProc extends MultipleNamedProcedureN {
        public ListOutputProc(String[] names) {
            super(names);
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            List<MetroPort> list = Pulsar.getCurrent().getOutputPorts();
            Collections.reverse( list );
            return LList.makeList( list  );
        }
    }
    static{ init( listOutputProc , listOutputBean ); } 

    public static final LamuDocument listInputBean =  listPortsTemplateBean.processArguments( "input" ).setNames("list-input" , "lsi");

    public static final Procedure listInputProc = new ListInputProc(new String[] { "list-input", "lsi" });
    public static final class ListInputProc extends MultipleNamedProcedureN {
        public ListInputProc(String[] names) {
            super(names);
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            List<MetroPort> list = Pulsar.getCurrent().getInputPorts();
            Collections.reverse( list );
            return LList.makeList( list  );
        }
    }
    static{ init( listInputProc , listInputBean ); } 

    public static final ConnectionTemplateBean connectTemplateBean =  new ConnectionTemplateBean();
    public static final class ConnectionTemplateBean extends PulsarProceduralDescriptiveBean {{
        setCategory( DOCS_ID );
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

    public static final LamuDocument connectBean =  connectTemplateBean.processArguments( "connects" ).setNames( "connect" );

    public static final ConnectProc connectProc = new ConnectProc(new String[] { "connect" });
    public static final class ConnectProc extends MultipleNamedProcedureN {
        public ConnectProc(String[] names) {
            super(names);
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            Pulsar.connectProc( Pulsar.getCurrent(), args, Pulsar.ConnectProc.CONNECT );
            return SchemeUtils.NO_RESULT;
        }
    }
    static{ init( connectProc , connectBean ); } 

    public static final LamuDocument disconnectBean =  connectTemplateBean.processArguments( "disconnects" ).setNames( "disconnect" );

    public static final DisconnectProc disconnectProc = new DisconnectProc(new String[] { "disconnect" });
    public static final class DisconnectProc extends MultipleNamedProcedureN {
        public DisconnectProc(String[] names) {
            super(names);
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            Pulsar.connectProc(Pulsar.getCurrent(), args, Pulsar.ConnectProc.DISCONNECT );
            return SchemeUtils.NO_RESULT;
        }
    }
    static{ init( disconnectProc , disconnectBean ); } 

    public static final InitDocAllConnectionBean allConnectionTemplateBean =  new InitDocAllConnectionBean(); 
    public static class InitDocAllConnectionBean extends PulsarProceduralDescriptiveBean {{
        setCategory( DOCS_ID );
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

    public static final LamuDocument listAllOutputBean =  allConnectionTemplateBean.processArguments( "output" ).setNames("list-all-output", "lao");

    public static final ListAllOutputProc listAllOutputProc = new ListAllOutputProc(new String[] { "list-all-output", "lao" });
    public static final class ListAllOutputProc extends MultipleNamedProcedureN {
        public ListAllOutputProc(String[] names) {
            super(names);
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            return Pair.makeList( Pulsar.getCurrent().getAvailableOutputPorts().stream().map( (v)->SchemeUtils.toSchemeString(v) )
                .collect( Collectors.toList() ) );
        }
    }
    static{ init( listAllOutputProc , listAllOutputBean ); } 

    public static final LamuDocument listAllInputBean =  allConnectionTemplateBean.processArguments( "input" ).setNames("list-all-input", "lai");

    public static final ListAllInputProc listAllInputProc = new ListAllInputProc(new String[] { "list-all-input", "lai" });
    public static final class ListAllInputProc extends MultipleNamedProcedureN {
        public ListAllInputProc(String[] names) {
            super(names);
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            return Pair.makeList( Pulsar.getCurrent().getAvailableInputPorts().stream().map( (v)->SchemeUtils.toSchemeString(v) )
                .collect( Collectors.toList() ) );
        }
    }
    static{ init( listAllInputProc , listAllInputBean ); } 

    public static final SetMainProc setMainProc = new SetMainProc(new String[] { "set-main" });
    public static final class SetMainProc extends MultipleNamedProcedureN {
        public SetMainProc(String[] names) {
            super(names);
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            Pulsar.logInfo("set-main");
            if ( args.length == 1 ) {
                Procedure procedure = (Procedure)args[0];
                Pulsar.getCurrent().setMainProcedure( InvokableSchemeProcedure.createSecretarillyInvokable( procedure ) );
            } else {
                throw new RuntimeException( "invalid argument length" );
            }
            return SchemeUtils.NO_RESULT;
        }
    }

    public static final SetMainBean setMainBean =  new SetMainBean();
    public static final class SetMainBean extends PulsarProceduralDescriptiveBean {
        {
            setCategory( DOCS_ID );
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
    static{ init( setMainProc , setMainBean ); } 

    ///////////////////////////////////////////////////////

    public static final GetMainProc getMainProc = new GetMainProc(new String[] { "get-main" });
    public static final class GetMainProc extends MultipleNamedProcedure0 {
        public GetMainProc(String[] names) {
            super(names);
        }

        @Override
        public Object apply0() throws Throwable {
            return Pulsar.getCurrent().getMainProcedure();
        }
    }

    public static final GetMainBean getMainBean =  new GetMainBean();
    public static final class GetMainBean extends PulsarProceduralDescriptiveBean {
        {
            setCategory( DOCS_ID );
            setNames( "get-main" );
            setParameterDescription( "" );
            setReturnValueDescription( "::procedure" );
            setShortDescription( "retrieves the main procedure. " );
            setLongDescription( ""
                    + "See (help set-main) for further information. " );
        }
    }
    static{ init( getMainProc , getMainBean ); } 

    public static final SetPlayingProc setPlayingProc = new SetPlayingProc(new String[] { "set-playing", "p" });
    public static final class SetPlayingProc extends MultipleNamedProcedureN {
        public SetPlayingProc(String[] names) {
            super(names);
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            Pulsar pulsar = Pulsar.getCurrent();
            if ( args.length == 0 ) {
                pulsar.togglePlaying();
            } else if ( args.length == 1 ) {
                pulsar.setPlaying( (Boolean)args[0] );
            } else {
                throw new RuntimeException( "invalid argument length" );
            }
            return SchemeUtils.NO_RESULT;
        }
    }

    public static final SetPlayingBean setPlayingBean =  new SetPlayingBean();
    public static final class SetPlayingBean extends PulsarProceduralDescriptiveBean {
        {
            setCategory( DOCS_ID );
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

    static{ init( setPlayingProc , setPlayingBean ); } 

    public static final IsPlayingProc isPlayingProc = new IsPlayingProc(new String[] { "playing?" });
    public static final class IsPlayingProc extends MultipleNamedProcedureN {
        public IsPlayingProc(String[] names) {
            super(names);
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            return Pulsar.getCurrent().getPlaying();
        }
    }

    public static final IsPlayingBean isPlayingBean =  new IsPlayingBean();
    public static final class IsPlayingBean extends PulsarProceduralDescriptiveBean {
        {
            setCategory( DOCS_ID );
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
    static{ init( isPlayingProc , isPlayingBean ); } 

    public static final PlayProc playProc = new PlayProc(new String[] { "play" });
    public static final class PlayProc extends MultipleNamedProcedureN {
        public PlayProc(String[] names) {
            super(names);
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            Pulsar.getCurrent().setPlaying( true ); 
            return SchemeUtils.NO_RESULT;
        }
    }

    public static final PlayBean playBean =  new PlayBean();
    public static final class PlayBean extends PulsarProceduralDescriptiveBean {
        {
            setCategory( DOCS_ID );
            setNames( "play" );
            setParameterDescription( "" );
            setReturnValueDescription( "::void" );
            setShortDescription( "causes the sequencer to start playing." );
            setLongDescription( "See (help set-play) for further information."
                                + THROWS_AN_ERROR_IF_NOT_OPEN
                                 );
        }
    }
    static{ init( playProc , playBean ); } 

    public static final StopProc stopProc = new StopProc(new String[] { "stop" });
    public static final class StopProc extends MultipleNamedProcedureN {
        public StopProc(String[] names) {
            super(names);
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            Pulsar.getCurrent().setPlaying( false ); 
            return SchemeUtils.NO_RESULT;
        }
    }

    public static final StopBean stopBean =  new StopBean();
    public static final class StopBean extends PulsarProceduralDescriptiveBean {
        {
            setCategory( DOCS_ID );
            setNames( "stop"  );
            setParameterDescription( "" );
            setReturnValueDescription( "::void" );
            setShortDescription( "causes the sequencer to stop playing." );
            setLongDescription( 
                "See (help set-play) for further information."
                    + THROWS_AN_ERROR_IF_NOT_OPEN );
        }
    }
    static{ init( stopProc , stopBean ); } 

    public static final QuitProc quitProc = new QuitProc(new String[] { "quit" });
    public static final class QuitProc extends MultipleNamedProcedureN {
        public QuitProc(String[] names) {
            super(names);
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            long shutdownWaitNow;
            if ( 0 < args.length  ) {
                shutdownWaitNow = SchemeUtils.toLong( args[0] );
            } else {
                shutdownWaitNow = Pulsar.shutdownWait;
            }
            
            Pulsar pulsar = Pulsar.getCurrent();
            Thread t = new Thread() {
                @Override
                public void run() {
                    try {
                        Thread.sleep( shutdownWaitNow );
                    } catch (InterruptedException e) {
                        Pulsar.logWarn( e.getMessage() );
                    }
                    pulsar.getParentApplicationComponent().processQuit(); 
                }
            };
            t.start();
            
            return "Now Pulsar will shutdown in " + shutdownWaitNow + " milliseconds...";
        }
    }

    public static final QuitBean quitBean =  new QuitBean();
    public static final class QuitBean extends PulsarProceduralDescriptiveBean {
        {
            setCategory( DOCS_ID );
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
    static{ init( quitProc , quitBean ); } 

    public static final TapTempoProc tapTempoProc = new TapTempoProc(new String[] { "tap-tempo", "tapt" });
    public static final class TapTempoProc extends MultipleNamedProcedureN {
        public TapTempoProc(String[] names) {
            super(names);
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            Pulsar.logInfo( "Pulsar Scheme API: TAP-TEMPO" );
            Pulsar pulsar = Pulsar.getCurrent();
            double bpm = pulsar.getTempoTapper().tap( pulsar.getBeatsPerMinute() );
            return SchemeUtils.toSchemeNumber( bpm );
        }
    }

    public static final TapTempoBean tapTempoBean =  new TapTempoBean();
    public static final class TapTempoBean extends PulsarProceduralDescriptiveBean {
        {
            setCategory( DOCS_ID );
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
    static{ init( tapTempoProc , tapTempoBean ); } 

    public static final SetTempoProc setTempoProc = new SetTempoProc(new String[] { "set-tempo" });
    public static final class SetTempoProc extends MultipleNamedProcedureN {
        public SetTempoProc(String[] names) {
            super(names);
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            if ( 0 < args.length ) {
                double bpm = SchemeUtils.toDouble(args[0]);
                Pulsar.getCurrent().setBeatsPerMinute( bpm );
            }
   
            return SchemeUtils.NO_RESULT;
        }
    }

    public static final SetTempoBean setTempoBean =  new SetTempoBean();
    public static final class SetTempoBean extends PulsarProceduralDescriptiveBean {
        {
            setCategory( DOCS_ID );
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
    static{ init( setTempoProc , setTempoBean ); } 

    public static final Procedure resetProc = new ResetProc(new String[] { "reset" });
    public static final class ResetProc extends MultipleNamedProcedure0 {
        public ResetProc(String[] names) {
            super(names);
        }

        @Override
        public Object apply0() throws Throwable {
            Pulsar.getCurrent().reset();
            return SchemeUtils.NO_RESULT;
        }
    }

    public static final ResetBean resetBean =  new ResetBean();
    public static final class ResetBean extends PulsarProceduralDescriptiveBean {
        {
            setCategory( DOCS_ID );
            setNames("reset" );
            setParameterDescription( "" );
            setReturnValueDescription( "::void" );
            setShortDescription( "resets the environment object of Scheme interpreter, and close the current JACK connection. " );
            setLongDescription( ""
                                + "This procedure is supposed to be called interactively and "
                                + "is not supposed to be called from other procedures; a procedure which "
                                + "called the (reset) procedure will be deleted from the current environment object as well "
                                + "as other procedures and as a result, "
                                + "the procedure cannot call other procedures which are necessary to continue "
                                + "the process. "
                                + "" );
        }
    }
    static{ init( resetProc , resetBean ); } 

    public static final RewindProc rewindProc = new RewindProc(new String[] { "rewind" });
    public static final class RewindProc extends MultipleNamedProcedure0 {
        public RewindProc(String[] names) {
            super(names);
        }

        @Override
        public Object apply0() throws Throwable {
            Pulsar.getCurrent().rewind();
            return SchemeUtils.NO_RESULT;
        }
    }

    public static final RewindBean rewindBean =  new RewindBean();
    public static final class RewindBean extends PulsarProceduralDescriptiveBean {
        {
            setCategory( DOCS_ID );
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
    static{ init( rewindProc , rewindBean ); } 

    public static final Procedure simultaneousProc = new SimultaneousProc(new String[] { "simultaneous", "simul" });
    public static final class SimultaneousProc extends MultipleNamedProcedureN {
        public SimultaneousProc(String[] names) {
            super(names);
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            Pulsar pulsar = Pulsar.getCurrent();
            synchronized ( pulsar.getMetroLock() ) {
                try {
                    pulsar.enterTrackChangeBlock();
                    for ( int i=0; i<args.length; i++ ) {
                        Object arg = args[i];
                        if ( arg instanceof Procedure ) {
                            ((Procedure)arg).apply0();
                        } else {
                            Pulsar.logWarn( "The value in args[" + i + "] was not a procedure. Ignored. "  );
                        }
                    }
                } finally {
                    pulsar.leaveTrackChangeBlock();
                    pulsar.notifyTrackChange();
                }
            }
            return SchemeUtils.NO_RESULT;
        }
    }

    public static final SimultaneousBean simultaneousBean = new SimultaneousBean();
    public static final class SimultaneousBean extends PulsarProceduralDescriptiveBean {
        {
            setCategory( DOCS_ID );
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
    static{ init( simultaneousProc , simultaneousBean ); } 

    public static final Procedure getTrackProc = new GetTrackProc(new String[] { "get-track", "gett" });
    public static final class GetTrackProc extends MultipleNamedProcedureN {
        public GetTrackProc(String[] names) {
            super(names);
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            Pulsar current = Pulsar.getCurrent();
            ArrayList<MetroTrack> t = new ArrayList<>();
            for ( int i=0; i<args.length; i++ ) {
                Object arg = args[i];
                t.addAll( current.readParamSearchTrack( arg ) );
            }
            return Pair.makeList( t );
        }
    }

    public static final GetTrackBean getTrackBean = new GetTrackBean();
    public static final class GetTrackBean extends PulsarProceduralDescriptiveBean {
        {
            setCategory( DOCS_ID );
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
    static{ init( getTrackProc , getTrackBean ); } 

    public static final AboutTrackSpecBean aboutTrackSpecBean = new AboutTrackSpecBean();
    public static final class AboutTrackSpecBean extends PulsarProceduralDescriptiveBean {
        {
            setCategory( DOCS_ID );
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

    public static final Procedure newTrackProc = new NewTrackProc( new String[] { "new-track", "newt" });
    public static final class NewTrackProc extends MultipleNamedProcedureN {
        public NewTrackProc(String[] names) {
            super(names);
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            Object name;
            List<Object> tags;
            Procedure procedure;
   
            switch ( args.length  ){
                case 0 : 
                    throw new IllegalArgumentException();
                case 1 :
                    name = null;
                    tags = null;
                    procedure = Pulsar.readParamProcedure(args[0]);
                    break;
                case 2 : {
                    List<Object> lst = Pulsar.readParamTrackName( args[0] );
                    name = lst.remove(0);
                    tags = lst;
                    procedure = Pulsar.readParamProcedure(args[1]);
                    break;
                }   
                default :
                    throw new IllegalArgumentException();
            }
            return Pulsar.getCurrent().createTrack( name, tags, procedure );
        }
    }

    public static final NewTrackBean newTrackBean = new NewTrackBean();
    public static final class NewTrackBean extends PulsarProceduralDescriptiveBean {
        {
            setCategory( DOCS_ID );
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
    static{ init( newTrackProc , newTrackBean ); } 

    public static final Procedure newRecordingTrackProc = new NewRecordTrackProc( new String[] { "new-recording-track", "rect" });
    public static final class NewRecordTrackProc extends MultipleNamedProcedureN {
        public NewRecordTrackProc(String[] names) {
            super(names);
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            Pulsar current = Pulsar.getCurrent();
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
                    List<Object> lst = Pulsar.readParamTrackName( args[0] );
                    name = lst.remove(0);
                    tags = null;
                    {
                        List<MetroPort> ports = current.readParamPort( args[1], current.getInputPorts() );
                        if ( ports.size() == 0 )
                            throw new IllegalArgumentException("could not find input port " + args[1] );
                        inputPorts = ports; 
                    }
                    {
                        List<MetroPort> ports = current.readParamPort( args[2], current.getOutputPorts() );
                        if ( ports.size() == 0 )
                            throw new IllegalArgumentException("could not find output port " + args[2] );
                        outputPorts = ports; 
                    }
                    
                    if ( 3< args.length ) {
                        recordLength = SchemeUtils.toDouble( args[3] );
                    } else {
                        recordLength = -1;
                    }
                    if ( 4< args.length ) {
                        looper = SchemeUtils.toBoolean( args[4] );
                    } else {
                        looper = true;
                    }
                    break;
                }
                
                default :
                    throw new IllegalArgumentException();
            }
            return Pulsar.getCurrent().createRecordingTrack( name, tags, inputPorts, outputPorts, recordLength, looper );
        }
    }

    public static final NewRecordingTrackBean newRecordingTrackBean = new NewRecordingTrackBean();
    public static final class NewRecordingTrackBean extends PulsarProceduralDescriptiveBean {
        {
            setCategory( DOCS_ID );
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
    static{ init( newRecordingTrackProc , newRecordingTrackBean ); } 

    public static final AboutNotationBean aboutNotationBean = new AboutNotationBean();
    public static final class AboutNotationBean extends PulsarProceduralDescriptiveBean {
        {
            setCategory( DOCS_ID );
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

    public static final AboutIntroBean aboutIntroBean = new AboutIntroBean();
    public static final class AboutIntroBean extends PulsarProceduralDescriptiveBean {
        {
            setCategory( DOCS_ID );
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

    public static final PulsarProceduralDescriptiveBean trackManagementTemplateBean = /*init*/( new TrackManagementTemplateBean() );
    public static final class TrackManagementTemplateBean extends PulsarProceduralDescriptiveBean {
        {
            setCategory( DOCS_ID );
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

    public static abstract class TrackManagementProc extends MultipleNamedProcedureN  {
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
            Pulsar pulsar = Pulsar.getCurrent();
            switch ( args.length ) {
                case 0 :
                    throw new IllegalArgumentException();
                case 1 :
                    trackList     = pulsar.readParamCreateTrack( args[0] );
                    syncType      = MetroSyncType.IMMEDIATE;
                    syncTrackList = Collections.EMPTY_LIST;
                    syncOffset    = 0.0d;
                    break;
                case 2 :
                    trackList     = pulsar.readParamCreateTrack( args[0] );
                    syncType      = Pulsar.readParamSyncType( args[1] );
                    syncTrackList = Collections.EMPTY_LIST;
                    syncOffset    = 0.0d;
                    break;
                case 3 :
                    trackList     = pulsar.readParamCreateTrack( args[0] );
                    syncType      = Pulsar.readParamSyncType( args[1] );
                    syncTrackList = pulsar.readParamCreateTrack( args[2] );
                    syncOffset    = 0.0d;
                    break;
                case 4 :
                    trackList     = pulsar.readParamCreateTrack( args[0] );
                    syncType      = Pulsar.readParamSyncType( args[1] );
                    syncTrackList = pulsar.readParamCreateTrack( args[2] );
                    syncOffset    = Pulsar.readParamSyncOffset( args[3] );
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
//                return Pair.makeList( trackList );
////              return Values.empty;
            
            // I want it to get back (Sun, 03 Nov 2019 04:56:43 +0900)
//                return Values.empty;
            return LList.makeList( trackList );
        }
    }

    public static final LamuDocument removeTrackBean = trackManagementTemplateBean.processArguments( 
        "removes",
              ""
            + "The sequencer remove the specified track. Eventually the track stops playing. "
            + "And it gives the user some controls on "
            + "how it stops playing the track. "    
        ).setNames( "remove-track", "remt" );

    public static final Procedure removeTrackProc = new RemoveTrackProc(new String[] { "remove-track", "remt" });
    public static final class RemoveTrackProc extends TrackManagementProc {
        public RemoveTrackProc(String[] names) {
            super(names);
        }

        @Override
        void procTrack( List<MetroTrack> trackList, MetroSyncType syncType, MetroTrack syncTrack, double syncOffset ) {
            Pulsar.getCurrent().removeTrack(trackList, syncType, syncTrack, syncOffset);
        }
    }
    static{ init( removeTrackProc , removeTrackBean ); } 

    public static final LamuDocument putTrackBean = trackManagementTemplateBean.processArguments( 
        "put",
        ""
        + "The sequencer starts to play the added track and it gives the user some controls on "
        + "how it starts playing the track."    
    ).setNames( "put-track", "putt" );
        
    public static final Procedure putTrackProc = new PutTrackProc(new String[] { "put-track", "putt" });
    public static final class PutTrackProc extends TrackManagementProc {
        public PutTrackProc(String[] names) {
            super(names);
        }

        @Override
        void procTrack( List<MetroTrack> trackList, MetroSyncType syncType, MetroTrack syncTrack, double syncOffset ) {
            Pulsar.getCurrent().putTrack(trackList, syncType, syncTrack, syncOffset);
        }
    }
    static{ init( putTrackProc , putTrackBean ); } 

    
    public static final Procedure notifyTrackChangeProc = new NotifyTrackChangeProc(new String[] { "notify-track-change", "nott" });
    public static final class NotifyTrackChangeProc extends MultipleNamedProcedure0 {
        public NotifyTrackChangeProc(String[] names) {
            super(names);
        }

        @Override
        public Object apply0() throws Throwable {
            Pulsar.getCurrent().notifyTrackChange();
            return SchemeUtils.NO_RESULT;
        }
    }

    public static final NotifyTrackChangeBean notifyTrackChangeBean = new NotifyTrackChangeBean();
    public static final class NotifyTrackChangeBean extends PulsarProceduralDescriptiveBean {
        {
            setCategory( DOCS_ID );
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
    static{ init( notifyTrackChangeProc , notifyTrackChangeBean ); } 

    public static final ListTracksProc listTracksProc = new ListTracksProc(new String[] { "list-tracks", "lstt" });
    public static final class ListTracksProc extends MultipleNamedProcedure0 {
        public ListTracksProc(String[] names) {
            super(names);
        }

        @Override
        public Object apply0() throws Throwable {
            List<MetroTrack> tempAllTracks = Pulsar.getCurrent().replicateAllTracks(); 
            ArrayList<Object> list = new ArrayList<>( tempAllTracks.size() );
            for ( MetroTrack track :  tempAllTracks ) {
                list.add( track );
            }
            Collections.reverse(list);
            
            return Pair.makeList(list);
   
        }
    }

    public static final ListTracksBean listTracksBean = new ListTracksBean();
    public static final class ListTracksBean extends PulsarProceduralDescriptiveBean {
        {
            setCategory( DOCS_ID );
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
    static{ init( listTracksProc , listTracksBean ); } 

    public static final MultipleNamedProcedure0 clearTracksProc = new ClearTracksProc(new String[] { "clear-tracks", "clet" });
    public static final class ClearTracksProc extends MultipleNamedProcedure0 {
        public ClearTracksProc(String[] names) {
            super(names);
        }

        @Override
        public Object apply0() throws Throwable {
            Pulsar.getCurrent().clearTracks();
            return SchemeUtils.NO_RESULT;
        }
    }

    public static final ClearTracksBean clearTracksBean = new ClearTracksBean();
    public static final class ClearTracksBean extends PulsarProceduralDescriptiveBean {
        {
            setCategory( DOCS_ID );
            setNames( "clear-tracks", "clet" );
            setParameterDescription( "" );
            setReturnValueDescription( "::void" );
            setShortDescription( "||<name/>|| removes all tracks on the current sequencer immediately. " );
            setLongDescription( ""
                                + "" 
                             );
        }
    }
    static{ init( clearTracksProc , clearTracksBean ); } 

    public static final Procedure getMainTrackProc = new GetMainTrackProc(new String[] { "get-main-track", "getmt" });
    public static final class GetMainTrackProc extends MultipleNamedProcedure0 {
        public GetMainTrackProc(String[] names) {
            super(names);
        }

        @Override
        public Object apply0() throws Throwable {
            return SchemeUtils.javaNullCheck( Pulsar.getCurrent().getMainTrack() );
        }
    }

    public static final GetMainTrackBean getMainTrackBean = new GetMainTrackBean();
    public static final class GetMainTrackBean extends PulsarProceduralDescriptiveBean {
        {
            setCategory( DOCS_ID );
            setNames( "get-main-track", "getmt" );
            setParameterDescription( "" );
            setReturnValueDescription( "::void" );
            setShortDescription( "||<name/>|| retrieves the reference to the current main track." );
            setLongDescription( ""
                              + "" 
                             );
        }
    }
    static{ init( getMainTrackProc , getMainTrackBean ); } 

    public static final Procedure getTrackPositionProc = new GetTrackPositionProc(new String[] { "get-track-position", "gettp" });
    public static final class GetTrackPositionProc extends MultipleNamedProcedure1 {
        public GetTrackPositionProc(String[] names) {
            super(names);
        }

        @Override
        public Object apply1( Object arg1 ) throws Throwable {
            if ( Boolean.FALSE.equals( arg1 ) ) { 
                return Boolean.FALSE;
            } else {
                MetroTrack track = ((MetroTrack)arg1);
                double position=-1d;
                synchronized ( track.getMetroTrackLock() ) {
                    position = track.getTrackPosition();
                }
                
                return SchemeUtils.toSchemeNumber( position );
            }
        }
    }

    public static final GetTrackPositionBean getTrackPositionBean = new GetTrackPositionBean();
    public static final class GetTrackPositionBean extends PulsarProceduralDescriptiveBean {
        {
            setCategory( DOCS_ID );
            setNames( "get-track-position", "gettp" );
            setParameterDescription( "" );
            setReturnValueDescription( "::void" );
            setShortDescription( "||<name/>|| gets the current position of the given track." );
            setLongDescription( ""
                                + "" 
                             );
        }
    }
    static{ init( getTrackPositionProc , getTrackPositionBean ); } 

    public static final PrintStackTraceProc printStackTraceProc = new PrintStackTraceProc(new String[] { "print-stack-trace" });
    public static final class PrintStackTraceProc extends MultipleNamedProcedure0 {
        public PrintStackTraceProc(String[] names) {
            super(names);
        }

        @Override
        public Object apply0() throws Throwable {
            PrintStream out = null;
            ByteArrayOutputStream bout = null;
            try {
                bout = new ByteArrayOutputStream();
                out = new PrintStream( bout );
                new Throwable().printStackTrace( out );
                out.flush();
                String value = new String( bout.toByteArray(), Charset.defaultCharset() );
                value = Pattern.compile( "^\\s+", Pattern.MULTILINE ).matcher( value ).replaceAll( "" );
                return SchemeUtils.toSchemeString( value ) ;
            } finally {
                if ( bout != null )
                    bout.close();
                if ( out != null )
                    out.close();
            }
        }
    }

    public static final PrintStackTraceBean printStackTraceBean = new PrintStackTraceBean();
    public static final class PrintStackTraceBean extends PulsarProceduralDescriptiveBean {
        {
            setCategory( DOCS_ID );
            setNames( "print-stack-trace" );
            setParameterDescription( "" );
            setReturnValueDescription( "::void" );
            setShortDescription( "||<name/>|| returns the current stack trace as a string. " );
            setLongDescription( ""
                                + "" 
                             );
        }
    }
    static{ init( printStackTraceProc , printStackTraceBean ); } 

    public static final DisplayWarnProc displayWarnProc = new DisplayWarnProc(new String[] { "display-warn" });
    public static final class DisplayWarnProc extends MultipleNamedProcedure1 {
        public DisplayWarnProc(String[] names) {
            super(names);
        }

        @Override
        public Object apply1(Object arg1) throws Throwable {
            System.err.print( arg1 );
            return Values.empty;
        }
    }

    public static final DisplayWarnBean displayWarnBean = new DisplayWarnBean();
    public static final class DisplayWarnBean extends PulsarProceduralDescriptiveBean {
        {
            setCategory( DOCS_ID );
            setNames( "display-warn" );
            setParameterDescription( "any" );
            addParameter( 0, "value", "any", null, false , "" );
            setReturnValueDescription( "::void" );
            setShortDescription( "||<name/>|| output the specified value to the standard error stream. " );
            setLongDescription( ""
                                + "" 
                             );
        }
    }
    static{ init( displayWarnProc , displayWarnBean ); } 

    public static final NewlineWarnProc newlineWarnProc = new NewlineWarnProc(new String[] { "newline-warn" });
    public static final class NewlineWarnProc extends MultipleNamedProcedure0 {
        public NewlineWarnProc(String[] names) {
            super(names);
        }

        @Override
        public Object apply0() throws Throwable {
            System.err.println();
            return Values.empty;
        }
    }

    public static final NewlineWarnBean newlineWarnBean = new NewlineWarnBean();
    public static final class NewlineWarnBean extends PulsarProceduralDescriptiveBean {
        {
            setCategory( DOCS_ID );
            setNames( "newline-warn" );
            setParameterDescription( "" );
            setReturnValueDescription( "::void" );
            setShortDescription( "||<name/>|| output a line terminator to the standard error stream. " );
            setLongDescription( ""
                                + "" 
                             );
        }
    }
    static{ init( newlineWarnProc , newlineWarnBean ); } 

    public static final TypeofProc typeofProc = new TypeofProc(new String[] { "typeof" });
    public static final class TypeofProc extends MultipleNamedProcedureN {
        public TypeofProc(String[] names) {
            super(names);
        }

        public Object applyN(Object[] args) throws Throwable {
            if ( 0 < args.length  ) {
                if ( args[0] == null ) 
                    return SchemeUtils.toSchemeString( "null" );
                else
                    return SchemeUtils.toSchemeString( args[0].getClass().getName() );
            } else {
                return SchemeUtils.NO_RESULT;
            }
        }
    }

    public static final TypeofBean typeofBean = new TypeofBean();
    public static final class TypeofBean extends PulsarProceduralDescriptiveBean {
        {
            setCategory( DOCS_ID );
            setNames( "typeof" );
            setParameterDescription( "any" );
            addParameter( 0, "value", "any", null, false , "" );
            setReturnValueDescription( "::string" );
            setShortDescription( "||<name/>|| returns a Java class name of the specified value. " );
            setLongDescription( "In case the specified value is a ||null|| of Java, this procedure returns \"null\" as a string value. "
                                + "" 
                             );
        }
    }
    static{ init( typeofProc , typeofBean ); } 

    public static final MakeTimerProc makeTimerProc = new MakeTimerProc(new String[] { "schedule", "make-timer" });
    public static final class MakeTimerProc extends MultipleNamedProcedureN {
        public MakeTimerProc(String[] names) {
            super(names);
        }

        public Object apply2(Object arg1, Object arg2) {
            Runnable runnable = PulsarSharedTimer.createTimer( Pulsar.getCurrent(), 
                SchemeUtils.toInteger( arg1 ), 
                -1, 
                InvokableSchemeProcedure.createSecretarillyInvokable( (Procedure)arg2 ) );
            
            return new MultipleNamedProcedure0() {
                public Object apply0() throws Throwable {
                    runnable.run();
                    return Values.empty;
                };
            };
        }

        @Override
        public Object apply3(Object arg0, Object arg1,Object arg2 ) throws Throwable {
            Runnable runnable = PulsarSharedTimer.createTimer( Pulsar.getCurrent(), 
                SchemeUtils.toInteger( arg0 ), 
                SchemeUtils.toInteger( arg1 ), 
                InvokableSchemeProcedure.createSecretarillyInvokable( (Procedure)arg2 ) );
   
            return new MultipleNamedProcedure0() {
                public Object apply0() throws Throwable {
                    runnable.run();
                    return Values.empty;
                };
            };
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            if ( args.length == 2 ) {
                return apply2( args[0], args[1] );
            } else if ( args.length == 3 ) {
                return apply3( args[0], args[1], args[2] );
            } else {
                WrongArguments.checkArgCount( "schedure", 2, 3, args.length );
                return false;
            }
        }
    }

    public static final MakeTimerBean makeTimerBean = new MakeTimerBean();
    public static final class MakeTimerBean extends PulsarProceduralDescriptiveBean {
        {
            setCategory( DOCS_ID );
            setNames("make-timer" );
            setParameterDescription( "delay interval proc" );
            addParameter( 0, "delay",     "number",    null, false , "" );
            addParameter( 0, "interval",  "number",    null, false , "" );
            addParameter( 0, "callback",  "procedure", null, false , "" );
        
            setReturnValueDescription( "::procedure" );
            setShortDescription( "||<name/>|| creates a new timer object. " );
            setLongDescription( ""
                    + "This procedure registers the specified procedure as a callback procedure of the timer; "
                    + "the procedure will be called with the specified period and with the specified delay. "
                    + "The return value is a cancel procedure. When the cancel procedure is called, the timer stops calling the "
                    + "callback procedure. "
                    + "" 
            );
        }
    }
    static{ init( makeTimerProc , makeTimerBean ); } 

    public static final AddEventListenerProc addEventListenerProc = new AddEventListenerProc(new String[] { "add-event-listener" });
    public static final class AddEventListenerProc extends MultipleNamedProcedure3 {
        public AddEventListenerProc(String[] names) {
            super(names);
        }

        @Override
        public Object apply3(Object arg0, Object arg1, Object arg2 ) throws Throwable {
            if ( arg0 instanceof EventListenable ) {
                EventListenable listenable = (EventListenable) arg0;
                Object type = SchemeUtils.schemeStringToJavaString( arg1 );
                Procedure procedure = (Procedure) arg2;
                Pulsar.PulsarEventListener listener = new Pulsar.PulsarEventListener( Pulsar.getCurrent(), procedure );
                listenable.addEventListener( type, listener);
                return listener;
            } else {
                throw new IllegalArgumentException( "the target is not event-listenable " );
            }
        }
    }

    public static final AddEventListenerBean addEventListenerBean = new AddEventListenerBean();
    public static final class AddEventListenerBean extends PulsarProceduralDescriptiveBean {
        {
            setCategory( DOCS_ID );
            setNames( "add-event-listener" );
            addParameter( 0, "target",     "object",    null, false , "" );
            addParameter( 0, "event-type", "symbol",    null, false , "" );
            addParameter( 0, "callback",   "procedure", null, false , "" );
        
            setReturnValueDescription( "::void" );
            setShortDescription( "||<name/>|| registers the specified procedure as an event handler. " );
            setLongDescription( ""
                    + "" 
            );
        }
    }
    static{ init( addEventListenerProc , addEventListenerBean ); } 

    public static final RemoveEventListenerProc removeEventListenerProc = new RemoveEventListenerProc(new String[] { "remove-event-listener" });
    public static final class RemoveEventListenerProc extends MultipleNamedProcedure2 {
        public RemoveEventListenerProc(String[] names) {
            super(names);
        }

        @Override
        public Object apply2(Object arg0, Object arg1 ) throws Throwable {
            if ( arg0 instanceof EventListenable ) {
                EventListenable listenable = (EventListenable) arg0;
                EventListenable.Listener listener;
                
                if (arg1 instanceof EventListenable.Listener) {
                    listener = (EventListenable.Listener)arg1;
                } else if (arg1 instanceof Procedure ) {
                    listener = new Pulsar.PulsarEventListener( Pulsar.getCurrent(), (Procedure) arg1 );
                } else {
                    throw new IllegalArgumentException( "the argument is not a listener object." );
                }
                listenable.removeEventListener( listener );
                return Values.empty;
            } else {
                throw new IllegalArgumentException( "the target is not event-listenable " );
            }
        }
    }

    public static final RemoveEventListenerBean removeEventListenerBean = new RemoveEventListenerBean();
    public static final class RemoveEventListenerBean extends PulsarProceduralDescriptiveBean {
        {
            setCategory( DOCS_ID );
            setNames( "remove-event-listener" );
            addParameter( 0, "target",     "object",    null, false , "" );
            addParameter( 0, "callback",   "procedure", null, false , "" );
        
            setReturnValueDescription( "::void" );
            setShortDescription( "||<name/>|| unregisters the specified procedure as an event handler. " );
            setLongDescription( ""
                    + "" 
            );
        }
    }
    static{ init( removeEventListenerProc , removeEventListenerBean ); } 

    public static final LamuDocument isTrackBean=null;

    public static final IsTrackProc isTrackProc = new IsTrackProc(new String[] { "track?" });
    public static final class IsTrackProc extends MultipleNamedProcedure1 {
        public IsTrackProc(String[] names) {
            super(names);
        }

        @Override
        public Object apply1(Object arg0 ) throws Throwable {
            return arg0 instanceof MetroTrack;
        }
    }
    static{ init( isTrackProc , isTrackBean ); } 

    public static final LamuDocument trackToProcedureBean = null;

    public static final TrackToProcedureProc trackToProcedureProc = new TrackToProcedureProc(new String[] { "track->procedure" });
    public static final class TrackToProcedureProc extends MultipleNamedProcedure1 {
        public TrackToProcedureProc(String[] names) {
            super(names);
        }

        @Override
        public Object apply1(Object arg0 ) throws Throwable {
            if ( arg0 instanceof MetroTrack ) {
                return ((MetroTrack)arg0).getSequence();
            } else {
                throw new IllegalArgumentException( "the argument is not a track object." );
            }
        }
    }
    static{ init( trackToProcedureProc , trackToProcedureBean ); } 

    public static final LamuDocument applyTrackBean = null;
    
    public static final ApplyTrackProc applyTrackProc = new ApplyTrackProc(new String[] { "apply-track", "appt" });
    public static final class ApplyTrackProc extends MultipleNamedProcedureN {
        public ApplyTrackProc(String[] names) {
            super(names);
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            if ( args.length < 1 ) {
                throw new IllegalArgumentException("insufficient argument length (length<1)" );
            }
            Object arg0 = args[0];
            Object[] restArgs = Arrays.copyOfRange( args, 1, args.length );
            if ( arg0 instanceof Invokable ) {
                return ((Invokable)arg0).invoke( restArgs );
            } else {
                throw new IllegalArgumentException( "the argument is not an invokable object." );
            }
        }
    }
    static{ init( applyTrackProc , applyTrackBean ); } 

    public static final LamuDocument readTrackBean = null;

    public static final ReadTrackProc readTrackProc = new ReadTrackProc(new String[] { "read-track", "reat" });
    public static final class ReadTrackProc extends MultipleNamedProcedureN {
        public ReadTrackProc(String[] names) {
            super(names);
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            if ( args.length < 1 ) {
                throw new IllegalArgumentException("insufficient argument length (length<1)" );
            }
            Object arg0 = args[0];
            if ( arg0 instanceof SchemeSequenceReadable ) {
                return ((SchemeSequenceReadable)arg0).readMusic();
            } else { 
                throw new IllegalArgumentException( "the argument is not a readable track. " + arg0 );
            }
        }
    }
    static{ init( readTrackProc , readTrackBean ); } 
    
    public static final LamuDocument createProcessBean = null;

    public static final CreateProcessProc createProcessProc = new CreateProcessProc(new String[] { "create-process", "newp" });
    public static final class CreateProcessProc extends MultipleNamedProcedureN {
        public CreateProcessProc(String[] names) {
            super(names);
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
   //                List l =  new ArrayList( Arrays.asList( args ) );
   //                l.add( 0, Keyword.make( "directory" ) );
   //                l.add( 1, new File( System.getProperty( "user.dir" ) ) );
   //                args = l.toArray();
            
            List<String> list = SchemeUtils.anySchemeValueListToStringList( Arrays.asList(args) );
            ProcessBuilder sb = new ProcessBuilder( list );
            
            // XXX ??? (Tue, 24 Mar 2020 06:09:27 +0900) <<< This should be integrated.
            sb.directory( ((Path) Shell.currentLoadPath.get()).toFile() );
            
            // TODO the IO should be able to controlled. this is bogus.
            // REMOVED (Tue, 24 Mar 2020 05:20:12 +0900) >>>
            // sb.inheritIO();
            // REMOVED (Tue, 24 Mar 2020 05:20:12 +0900) <<<
            return new PulsarProcessWrapper( sb.start(), new ArrayList( list ) );
        }
    }
    static{ init( createProcessProc , createProcessBean ); } 

    public static final LamuDocument destroyProcessBean = null;

    public static final DestroyProcessProc destroyProcessProc = new DestroyProcessProc(new String[] { "destroy-process", "kilp" });
    public static final class DestroyProcessProc extends MultipleNamedProcedureN {
        public DestroyProcessProc(String[] names) {
            super(names);
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            for ( int i=0; i<args.length; i++ ) {
                if ( args[i] instanceof Process ) {
                    ((Process)args[i]).destroy();
                } else if ( args[i] instanceof PulsarProcessWrapper ) {
                        ((PulsarProcessWrapper)args[i]).destroy();
                } else {
                    Pulsar.logWarn( "warning : the value of the arguments no " + i + " is not a process object." );
                }
            }
            return Values.empty;
        }
    }
    static{ init( destroyProcessProc , destroyProcessBean ); } 

    public static final LamuDocument killProcessBean = null;

    public static final KillProcessProc killProcessProc = new KillProcessProc(new String[] { "kill-process", "fkilp" });
    public static final class KillProcessProc extends MultipleNamedProcedureN {
        public KillProcessProc(String[] names) {
            super(names);
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            for ( int i=0; i<args.length; i++ ) {
                if ( args[i] instanceof Process ) {
                    ((Process)args[i]).destroyForcibly();
                } else if ( args[i] instanceof PulsarProcessWrapper ) {
                    ((PulsarProcessWrapper)args[i]).destroyForcibly();
                } else {
                    Pulsar.logWarn( "warning : the value of the arguments no " + i + " is not a process object." );
                }
            }
            return Values.empty;
        }
    }
    static{ init( killProcessProc , killProcessBean ); } 

    public static final LamuDocument sleepBean = null;

    public static final SleepProc sleepProc = new SleepProc(new String[] { "sleep" });
    public static final class SleepProc extends MultipleNamedProcedure1 {
        public SleepProc(String[] names) {
            super(names);
        }

        @Override
        public Object apply1(Object arg1) throws Throwable {
            Thread.sleep( SchemeUtils.toInteger( arg1 ));
            return Values.empty;
        }
    }
    static{ init( sleepProc , sleepBean ); } 

    public static final RandomBean randomBean = new RandomBean();
    public static final class RandomBean extends PulsarProceduralDescriptiveBean {
        {
            setCategory( DOCS_ID );
            setNames( "random", "rnd" );
            setParameterDescription( "[range::number]" );
            addParameter( 0, "range",     "number",  "1",  false , "" );
            setReturnValueDescription( "::number" );
            setShortDescription( "||<name/>|| generates a random number. " );
            setLongDescription( ""
                    + "This procedure adopts Mersenne Twister a random number generating algorithm. "
                    + "If an argument [range] is specified, the return value will be within 0<= x <[range]. "
                    + "If the argument is omitted, the range value defaults to 1. "
                    + "" 
            );
        }
    }
    public static final RandomProc randomProc = new RandomProc(new String[] { "random", "rnd" });
    public static final class RandomProc extends MultipleNamedProcedureN {
        public RandomProc(String[] names) {
            super(names);
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            Pulsar pulsar = Pulsar.getCurrent();
            switch ( args.length ) {
                case 0 :
                    return DFloNum.valueOf( pulsar.random.nextDouble() );
                case 1 : {
                    double range = SchemeUtils.toDouble( args[0] );
                    return DFloNum.valueOf( pulsar.random.nextDouble() * range );
                }
                default :
                {
                    double rangeMin = SchemeUtils.toDouble( args[0] );
                    double rangeMax = SchemeUtils.toDouble( args[1] );
                    double range    = rangeMax - rangeMin;
   
                    return DFloNum.valueOf( pulsar.random.nextDouble() * range + rangeMin );
                }
            }
        }
    }
    static{ init( randomProc , randomBean ); } 

    public static final LuckBean luckBean = new LuckBean();
    public static final class LuckBean extends PulsarProceduralDescriptiveBean {
        {
            setCategory( DOCS_ID );
            setNames( "luck" );
            setParameterDescription( "[numeric]" );
            addParameter( 0, "probability",   "number",  "0.5",  false, "the probability to return #t." );
            setReturnValueDescription( "::boolean" );
            setShortDescription( "||<name/>|| is a procedure that returns a random boolean value. " );
            setLongDescription( "The first argument is the value of probability "
                    + "where the larger value causes the more probability of returning #t. "
                    + "When the specified value is equals or less than zero, the returning value is always #f. "
                    + "When the specified value is equals or larger than one the returning value is always #t. "
                    + "The only parameter can be omitted and in that case the default value one is applied. " );
        }
    }

    public static final LuckProc luckProc = new LuckProc( "luck" );
    public static final class LuckProc extends MultipleNamedProcedureN {
        public LuckProc(String ... names) {
            super(names);
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            double probability = args.length == 0 ? 0.5 : SchemeUtils.toDouble( args[0] );
            if ( probability < 0 ) return false;
            if ( 1.0<=probability  ) return true;
            return Pulsar.getCurrent().random.nextBoolean( probability );
        }
    }
    static{ init( luckProc , luckBean ); } 



    /**
     * Initializes an environment of scheme engine and defines API for the scripts.
     * 
     * @param scheme
     *            the scheme instance to initialize.
     */
    
    public static void initScheme( Environment env ) {
        SchemeUtils.defineLambda( env, pulsarProc );
        SchemeUtils.defineLambda( env, isPulsarPresentProc );
        SchemeUtils.defineLambda( env, isOpenProc );
        // PulsarDocuments.DOCS.defineDoc( env, isOpenBean );
        SchemeUtils.defineLambda( env, openProc);
        // PulsarDocuments.DOCS.defineDoc( env, openBean );
        SchemeUtils.defineLambda( env, closeProc);
        // PulsarDocuments.DOCS.defineDoc( env, closeBean );
        SchemeUtils.defineLambda( env, openOutputProc );
        // PulsarDocuments.DOCS.defineDoc( env, openOutputBean );
        SchemeUtils.defineLambda( env, openInputProc  );
        // PulsarDocuments.DOCS.defineDoc( env, openInputBean );
        SchemeUtils.defineLambda( env, closeOutputProc );
        // PulsarDocuments.DOCS.defineDoc( env, closeOutputBean );
        SchemeUtils.defineLambda( env, closeInputProc );
        // PulsarDocuments.DOCS.defineDoc( env, closeInputBean );

        //////////////////////////////////////////////////////////

 
        SchemeUtils.defineLambda( env, listOutputProc );
        // PulsarDocuments.DOCS.defineDoc( env, listOutputBean );
        
        //////////////////////////////////////////////////////////

        SchemeUtils.defineLambda( env, listInputProc );
        // PulsarDocuments.DOCS.defineDoc( env, listInputBean );

        //////////////////////////////////////////////////////////
        
        SchemeUtils.defineLambda( env, connectProc );
        // PulsarDocuments.DOCS.defineDoc( env, connectBean );
        
        //////////////////////////////////////////////////////////
        
        SchemeUtils.defineLambda( env, disconnectProc);
        // PulsarDocuments.DOCS.defineDoc( env, disconnectBean );

        //////////////////////////////////////////////////////////
        
        SchemeUtils.defineLambda( env, listAllOutputProc);
        // PulsarDocuments.DOCS.defineDoc( env, listAllOutputBean );

        //////////////////////////////////////////////////////////
        
        SchemeUtils.defineLambda( env, listAllInputProc);
        // PulsarDocuments.DOCS.defineDoc( env, listAllInputBean );

        //////////////////////////////////////////////////////////
        
        
        SchemeUtils.defineLambda( env, setMainProc);
        // PulsarDocuments.DOCS.defineDoc( env, setMainBean );

        //////////////////////////////////////////////////////////

        SchemeUtils.defineLambda( env, getMainProc);
        // PulsarDocuments.DOCS.defineDoc( env, getMainBean );

        //////////////////////////////////////////////////////////
        
        SchemeUtils.defineLambda( env, setPlayingProc);

        // PulsarDocuments.DOCS.defineDoc( env, setPlayingBean );

        SchemeUtils.defineLambda( env, isPlayingProc);

        // PulsarDocuments.DOCS.defineDoc( env, isPlayingBean );
        
        SchemeUtils.defineLambda( env, playProc);
        
        // PulsarDocuments.DOCS.defineDoc( env, playBean );

        SchemeUtils.defineLambda( env, stopProc);

        // PulsarDocuments.DOCS.defineDoc( env, stopBean );

        SchemeUtils.defineLambda( env, quitProc  );
        
        // PulsarDocuments.DOCS.defineDoc( env, quitBean);
        
        SchemeUtils.defineLambda( env, tapTempoProc);
        
        // PulsarDocuments.DOCS.defineDoc( env, tapTempoBean );

        SchemeUtils.defineLambda( env, setTempoProc);
        
        // PulsarDocuments.DOCS.defineDoc( env, setTempoBean );
        
        //////////////////////////////////////////////////////////

        /**
         * This function only reset the current scheme environment.
         * See {@link Pulsar#reset }
         */
        SchemeUtils.defineLambda( env, resetProc );
        // PulsarDocuments.DOCS.defineDoc( env, resetBean );
        
        SchemeUtils.defineLambda( env, rewindProc);
        
        // PulsarDocuments.DOCS.defineDoc( env, rewindBean );
        
        SchemeUtils.defineLambda( env, simultaneousProc );
        // PulsarDocuments.DOCS.defineDoc( env, simultaneousBean );
        
        /////////////////////////////////////////////////////////////////

        SchemeUtils.defineLambda( env, getTrackProc );
        // PulsarDocuments.DOCS.defineDoc( env, getTrackBean );
        
        /////////////////////////////////////////////////////////////////

        // PulsarDocuments.DOCS.defineDoc( env, aboutTrackSpecBean );
        
        SchemeUtils.defineLambda( env, newTrackProc );
        // PulsarDocuments.DOCS.defineDoc( env, newTrackBean );
        
        SchemeUtils.defineLambda( env, newRecordingTrackProc );
        // PulsarDocuments.DOCS.defineDoc( env, newRecordTrackBean );
        
        
        
        /////////////////////////////////////////////////////////////////
        // ( canonical )
        // PulsarDocuments.DOCS.defineDoc( env, aboutNotationBean );
        
        // ( canonical )
        // PulsarDocuments.DOCS.defineDoc( env, aboutIntroBean );

        /////////////////////////////////////////////////////////////////

        SchemeUtils.defineLambda( env, putTrackProc );
        // PulsarDocuments.DOCS.defineDoc( env, putTrackBean);

        /////////////////////////////////////////////////////////////////

        SchemeUtils.defineLambda( env, removeTrackProc );
        // PulsarDocuments.DOCS.defineDoc( env, removeTrackBean );

        SchemeUtils.defineLambda( env, notifyTrackChangeProc );
        // PulsarDocuments.DOCS.defineDoc( env, notifyTrackChangeBean );
        
        SchemeUtils.defineLambda( env, listTracksProc);
        // PulsarDocuments.DOCS.defineDoc( env, listTracksBean );       
        
        SchemeUtils.defineLambda( env, clearTracksProc );
        
        // PulsarDocuments.DOCS.defineDoc( env, clearTracksBean );   

        SchemeUtils.defineLambda( env, getMainTrackProc );
        
        // PulsarDocuments.DOCS.defineDoc( env, getMainTrackBean );   
        
        SchemeUtils.defineLambda( env, getTrackPositionProc );
        
        // PulsarDocuments.DOCS.defineDoc( env, getTrackPositionBean );   
        
        SchemeUtils.defineLambda( env, printStackTraceProc);
        
        // PulsarDocuments.DOCS.defineDoc( env, printStackTraceBean );   

        
        SchemeUtils.defineLambda( env, displayWarnProc);

        // PulsarDocuments.DOCS.defineDoc( env, displayWarnBean );
        
        SchemeUtils.defineLambda( env, newlineWarnProc);

        // PulsarDocuments.DOCS.defineDoc( env, newlineWarnBean );
        
        SchemeUtils.defineLambda( env, typeofProc);
        
        // PulsarDocuments.DOCS.defineDoc( env, typeofBean );

        SchemeUtils.defineLambda( env, makeTimerProc);

        // PulsarDocuments.DOCS.defineDoc( env, makeTimerBean );
        
        
        SchemeUtils.defineLambda( env, addEventListenerProc );

        // PulsarDocuments.DOCS.defineDoc( env, addEventListenerBean );
        SchemeUtils.defineLambda( env, removeEventListenerProc );
        // PulsarDocuments.DOCS.defineDoc( env, removeEventListenerBean);
        SchemeUtils.defineLambda( env, isTrackProc);
        SchemeUtils.defineLambda( env, trackToProcedureProc);
        SchemeUtils.defineLambda( env, applyTrackProc);
        SchemeUtils.defineLambda( env, readTrackProc);
        SchemeUtils.defineLambda( env, createProcessProc);
        SchemeUtils.defineLambda( env, destroyProcessProc); 
        SchemeUtils.defineLambda( env, killProcessProc);
        SchemeUtils.defineLambda( env, sleepProc );
        SchemeUtils.defineLambda( env, randomProc);
        // PulsarDocuments.DOCS.defineDoc( env, randomBean );
        SchemeUtils.defineLambda( env, luckProc);
        
        // ???
//      SchemeUtils.defineDoc( scheme,
//          new DescriptiveInitializerBean(){{
//              setParameterDescription( "[probability::number]" );
//              addParameter("probability",   "number",  "0.5",  false , "the probability to return #t." );
//              setReturnValueDescription( "::bool" );
//              setShortDescription( "||<name/>|| returns a boolean value randomly. " );
//              setLongDescription( ""
//                      + "The only parameter is the probability. If it is zero, the result is always #f. "
//                      + "And if the argument is one, the result is always #t. "
//                      + "" 
//              );
//          }}, 
//          "luck" );


        // PulsarDocuments.DOCS.defineDoc( env, luckBean );
        PulsarLib_Notes.defineDoc( env, PulsarNoteListParser.getInstance() );

//        try {
//            SchemeEvaluator evaluator = new SchemeEvaluator( scheme );
//            evaluator.evaluate( Pulsar.class, "lib/init.scm"  ).warnIfError();
//            evaluator.evaluate( Pulsar.class, "lib/basic-notes.scm"  ).warnIfError();
//            evaluator.evaluate( Pulsar.class, "lib/music.scm"  ).warnIfError();
//            evaluator.evaluate( Pulsar.class, "lib/xnoop.scm" ).warnIfError();
//        } catch ( Throwable t ) {
//            logError( "", t );
//        }
    }
}
