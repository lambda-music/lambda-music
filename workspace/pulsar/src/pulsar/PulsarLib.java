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
import lamu.lib.scheme.InvokableSchemeProcedure;
import lamu.lib.scheme.SchemeUtils;
import lamu.lib.scheme.doc.ProceduralDescriptiveBean;
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

public class PulsarLib {
    public static final Environment env = null;

    static final String THROWS_AN_ERROR_IF_NOT_OPEN = 
        "In case the current sequencer system has not established any connection to the JACK, " + 
            "it throws an exception. ";

    static final String ALTERS_THE_CURRENT_STATE =
        "This procedure alters the current sequencer system's state. ";

    /**
         * Initializes an environment of scheme engine and defines API for the scripts.
         * 
         * @param scheme
         *            the scheme instance to initialize.
         */
        public static void initScheme( Environment env ) {
    
            SchemeUtils.defineLambda(env, new MultipleNamedProcedure0( "pulsar" ) {
                @Override
                public Object apply0() throws Throwable {
                    return Pulsar.getCurrent();
                }
            });
    
            SchemeUtils.defineLambda(env, new MultipleNamedProcedure0( "pulsar-present?" ) {
                @Override
                public Object apply0() throws Throwable {
                    return Pulsar.isPresent();
                }
            });
    
    
            SchemeUtils.defineLambda( env, new MultipleNamedProcedureN( "open?" ) {
                @Override
                public Object applyN(Object[] args) throws Throwable {
                    return Pulsar.getCurrent().isOpened();
                }
            });
            
            PulsarDocuments.DOCS.defineDoc( env, new ProceduralDescriptiveBean() {{
                setNames( "open?" );
                setParameterDescription( "" );
                setReturnValueDescription( "::boolean" );
                setShortDescription(       "||<name/>|| returns the current open state. " );
                setLongDescription(        "This procedure returns #t iff the current sequencer state is open; "
                                         + "otherwise returns #f. " );
            }} );
            
            SchemeUtils.defineLambda( env, new MultipleNamedProcedure1( "open" ) {
                @Override
                public Object apply1(Object arg0) throws Throwable {
                    Pulsar.getCurrent().open( SchemeUtils.toString( arg0 ) );
                    return SchemeUtils.NO_RESULT;
                }
            });
            
            PulsarDocuments.DOCS.defineDoc( env, new ProceduralDescriptiveBean() {{
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
            }} );
            
            SchemeUtils.defineLambda( env, new MultipleNamedProcedureN( "close" ) {
                @Override
                public Object applyN(Object[] args) throws Throwable {
                    Pulsar.getCurrent().close();
                    return SchemeUtils.NO_RESULT;
                }
            });
    
            PulsarDocuments.DOCS.defineDoc( env, new ProceduralDescriptiveBean() {{
                setNames( "close" );
                setParameterDescription( "" );
                setReturnValueDescription( "::void" );
                setShortDescription( "ends the current connection between JACK Audio Connection Kit." );
                setLongDescription( "This procedure closes the current connection to the JACK Audio Connection Kit. "
                        + "When it failed to close the connection, this throws an exception. "
                        + THROWS_AN_ERROR_IF_NOT_OPEN
                        + ALTERS_THE_CURRENT_STATE );
            }} );
    
            //////////////////////////////////////////////////////////
    
            ProceduralDescriptiveBean initDocOpenPorts = new ProceduralDescriptiveBean() {{
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
            }};     
            
            Procedure openOutput = new MultipleNamedProcedureN( "open-output", "openo") {
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
            };
            SchemeUtils.defineLambda( env, openOutput );
    
            PulsarDocuments.DOCS.defineDoc( env, initDocOpenPorts.processArguments( "output" ).setNames( "open-output", "openo" ) );
            
            //////////////////////////////////////////////////////////
    
            Procedure openInput = new MultipleNamedProcedureN( "open-input", "openi" ) {
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
            };
            SchemeUtils.defineLambda( env, openInput  );
    
            PulsarDocuments.DOCS.defineDoc( env, initDocOpenPorts.processArguments( "input" ).setNames( "open-input" , "openi" ) );
    
            
            //////////////////////////////////////////////////////////
            
            class InitDocClosePorts extends ProceduralDescriptiveBean {{
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
            InitDocClosePorts initDocClosePorts = new InitDocClosePorts();
            
            Procedure closeOutput = new MultipleNamedProcedureN( "close-output", "closeo" ) {
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
            };
            SchemeUtils.defineLambda( env, closeOutput );
            PulsarDocuments.DOCS.defineDoc( env, initDocClosePorts.processArguments( "output" ).setNames( "close-output" , "closeo" ) );
    
            //////////////////////////////////////////////////////////
            
            Procedure closeInput = new MultipleNamedProcedureN( "close-input", "closei" ) {
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
            };
            SchemeUtils.defineLambda( env, closeInput );
            
            PulsarDocuments.DOCS.defineDoc( env, initDocClosePorts.processArguments( "input" ).setNames( "close-input", "closei" ) );
    
            //////////////////////////////////////////////////////////
    
            class InitDocListPorts extends ProceduralDescriptiveBean {{
                    setParameterDescription( "" );
                    setReturnValueDescription( "::(list MetroPort ...)" );
                    setShortDescription( "returns a list which contains all %s ports on the current JACK connection. " );
                    setLongDescription( ""
                                        + "Each element on the list is a reference to a MetroPort object. "
                                        + "The values in the list are sorted from newest to oldest. "
                                        + THROWS_AN_ERROR_IF_NOT_OPEN
                                        );
            }}
            InitDocListPorts initDocListPorts = new InitDocListPorts();
     
            Procedure listOutput = new MultipleNamedProcedureN( "list-output", "lso" ) {
                @Override
                public Object applyN(Object[] args) throws Throwable {
                    List<MetroPort> list = Pulsar.getCurrent().getOutputPorts();
                    Collections.reverse( list );
                    return LList.makeList( list  );
                }
            };
            SchemeUtils.defineLambda( env, listOutput );
            PulsarDocuments.DOCS.defineDoc( env, initDocListPorts.processArguments( "output" ).setNames("list-output" , "lso" ) );
            
            
            //////////////////////////////////////////////////////////
    
            Procedure listInput = new MultipleNamedProcedureN( "list-input", "lsi" ) {
                @Override
                public Object applyN(Object[] args) throws Throwable {
                    List<MetroPort> list = Pulsar.getCurrent().getInputPorts();
                    Collections.reverse( list );
                    return LList.makeList( list  );
                }
            };
            SchemeUtils.defineLambda( env, listInput );
            PulsarDocuments.DOCS.defineDoc( env, initDocListPorts.processArguments( "input" ).setNames("list-input" , "lsi") );
    
            //////////////////////////////////////////////////////////
    
            class InitDocConnection extends ProceduralDescriptiveBean {{
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
            InitDocConnection initDocConnection = new InitDocConnection();
            
            SchemeUtils.defineLambda( env, new MultipleNamedProcedureN( "connect" ) {
                @Override
                public Object applyN(Object[] args) throws Throwable {
                    Pulsar.connectProc( Pulsar.getCurrent(), args, Pulsar.ConnectProc.CONNECT );
                    return SchemeUtils.NO_RESULT;
                }
            } );
            
            PulsarDocuments.DOCS.defineDoc( env, initDocConnection.processArguments( "connects" ).setNames( "connect" ) );
            
            //////////////////////////////////////////////////////////
            
            SchemeUtils.defineLambda( env, new MultipleNamedProcedureN( "disconnect" ) {
                @Override
                public Object applyN(Object[] args) throws Throwable {
                    Pulsar.connectProc(Pulsar.getCurrent(), args, Pulsar.ConnectProc.DISCONNECT );
                    return SchemeUtils.NO_RESULT;
                }
            });
    
            PulsarDocuments.DOCS.defineDoc( env, initDocConnection.processArguments( "disconnects" ).setNames( "disconnect" ) );
    
            //////////////////////////////////////////////////////////
            
            class InitDocAllConnection extends ProceduralDescriptiveBean {{
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
            InitDocAllConnection initDocAllConnection = new InitDocAllConnection(); 
            SchemeUtils.defineLambda( env, new MultipleNamedProcedureN( "list-all-output", "lao" ) {
                @Override
                public Object applyN(Object[] args) throws Throwable {
                    return Pair.makeList( Pulsar.getCurrent().getAvailableOutputPorts().stream().map( (v)->SchemeUtils.toSchemeString(v) )
                        .collect( Collectors.toList() ) );
                }
            });
    
            PulsarDocuments.DOCS.defineDoc( env, initDocAllConnection.processArguments( "output" ).setNames("list-all-output", "lao") );
    
            //////////////////////////////////////////////////////////
            
            SchemeUtils.defineLambda( env, new MultipleNamedProcedureN( "list-all-input", "lai" ) {
                @Override
                public Object applyN(Object[] args) throws Throwable {
                    return Pair.makeList( Pulsar.getCurrent().getAvailableInputPorts().stream().map( (v)->SchemeUtils.toSchemeString(v) )
                        .collect( Collectors.toList() ) );
                }
            });
            
            PulsarDocuments.DOCS.defineDoc( env, initDocAllConnection.processArguments( "input" ).setNames("list-all-input", "lai") );
    
            //////////////////////////////////////////////////////////
            
            
            SchemeUtils.defineLambda( env, new MultipleNamedProcedureN( "set-main" ) {
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
            });
    
            PulsarDocuments.DOCS.defineDoc( env, new ProceduralDescriptiveBean() {{
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
            }} );
    
            //////////////////////////////////////////////////////////
    
            SchemeUtils.defineLambda( env, new MultipleNamedProcedure0( "get-main" ) {
                @Override
                public Object apply0() throws Throwable {
                    return Pulsar.getCurrent().getMainProcedure();
                }
            });
            
            PulsarDocuments.DOCS.defineDoc( env, new ProceduralDescriptiveBean() {{
                setNames( "get-main" );
                setParameterDescription( "" );
                setReturnValueDescription( "::procedure" );
                setShortDescription( "retrieves the main procedure. " );
                setLongDescription( ""
                        + "See (help set-main) for further information. " );
            }} );
    
            //////////////////////////////////////////////////////////
            
            SchemeUtils.defineLambda( env, new MultipleNamedProcedureN( "set-playing", "p" ) {
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
            });
    
            PulsarDocuments.DOCS.defineDoc( env, new ProceduralDescriptiveBean(){{
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
            }} );
    
            SchemeUtils.defineLambda( env, new MultipleNamedProcedureN( "playing?" ) {
                @Override
                public Object applyN(Object[] args) throws Throwable {
                    return Pulsar.getCurrent().getPlaying();
                }
            });
    
            PulsarDocuments.DOCS.defineDoc( env, new ProceduralDescriptiveBean(){{
                setNames( "playing?" );
                setParameterDescription( "" );
                setReturnValueDescription( "::boolean" );
                setShortDescription( "retrieves the current playing state." );
                setLongDescription( ""
                                    + "When the sequencer is playing, it returns #t; otherwise it returns #f. "
                                    + "See (help set-playing) for further information. "
                                    + THROWS_AN_ERROR_IF_NOT_OPEN
                                     );
            }} );
            
            SchemeUtils.defineLambda( env, new MultipleNamedProcedureN( "play" ) {
                @Override
                public Object applyN(Object[] args) throws Throwable {
                    Pulsar.getCurrent().setPlaying( true ); 
                    return SchemeUtils.NO_RESULT;
                }
            });
            
            PulsarDocuments.DOCS.defineDoc( env, new ProceduralDescriptiveBean(){{
                setNames( "play" );
                setParameterDescription( "" );
                setReturnValueDescription( "::void" );
                setShortDescription( "causes the sequencer to start playing." );
                setLongDescription( "See (help set-play) for further information."
                                    + THROWS_AN_ERROR_IF_NOT_OPEN
                                     );
            }} );
    
            SchemeUtils.defineLambda( env, new MultipleNamedProcedureN( "stop" ) {
                @Override
                public Object applyN(Object[] args) throws Throwable {
                    Pulsar.getCurrent().setPlaying( false ); 
                    return SchemeUtils.NO_RESULT;
                }
            });
    
            PulsarDocuments.DOCS.defineDoc( env, new ProceduralDescriptiveBean(){{
                setNames( "stop"  );
                setParameterDescription( "" );
                setReturnValueDescription( "::void" );
                setShortDescription( "causes the sequencer to stop playing." );
                setLongDescription( 
                    "See (help set-play) for further information."
                        + THROWS_AN_ERROR_IF_NOT_OPEN );
            }} );
    
            SchemeUtils.defineLambda( env, new MultipleNamedProcedureN( "quit" ) {
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
            }  );
            
            PulsarDocuments.DOCS.defineDoc( env, new ProceduralDescriptiveBean(){{
                setNames( "quit"  );
                setParameterDescription( "" );
                setReturnValueDescription( "::void" );
                setShortDescription( "quits the application. " );
                setLongDescription( ""
                                    + "makes the sequencer to stop playing "
                                    + "and shutdowns the application in " + Pulsar.shutdownWait + " milliseconds. "
                                    + "Currently the time to shutdown is hard-coded and cannot be changed. "
                                    + THROWS_AN_ERROR_IF_NOT_OPEN );
            }});
            
            SchemeUtils.defineLambda( env, new MultipleNamedProcedureN( "tap-tempo", "tapt" ) {
                @Override
                public Object applyN(Object[] args) throws Throwable {
                    Pulsar.logInfo( "Pulsar Scheme API: TAP-TEMPO" );
                    Pulsar pulsar = Pulsar.getCurrent();
                    double bpm = pulsar.getTempoTapper().tap( pulsar.getBeatsPerMinute() );
                    return SchemeUtils.toSchemeNumber( bpm );
                }
            });
            
            PulsarDocuments.DOCS.defineDoc( env, new ProceduralDescriptiveBean(){{
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
            }} );
    
            SchemeUtils.defineLambda( env, new MultipleNamedProcedureN( "set-tempo" ) {
                @Override
                public Object applyN(Object[] args) throws Throwable {
                    if ( 0 < args.length ) {
                        double bpm = SchemeUtils.toDouble(args[0]);
                        Pulsar.getCurrent().setBeatsPerMinute( bpm );
                    }
    
                    return SchemeUtils.NO_RESULT;
                }
            });
            
            PulsarDocuments.DOCS.defineDoc( env, new ProceduralDescriptiveBean(){{
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
            }} );
            
            //////////////////////////////////////////////////////////
    
            /**
             * This function only reset the current scheme environment.
             * See {@link Pulsar#reset }
             */
            Procedure resetScheme = new MultipleNamedProcedure0( "reset" ) {
                @Override
                public Object apply0() throws Throwable {
                    Pulsar.getCurrent().reset();
                    return SchemeUtils.NO_RESULT;
                }
            };
            SchemeUtils.defineLambda( env, resetScheme );
            PulsarDocuments.DOCS.defineDoc( env, new ProceduralDescriptiveBean(){{
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
            }} );
            
            
            SchemeUtils.defineLambda( env, new MultipleNamedProcedure0( "rewind" ) {
                @Override
                public Object apply0() throws Throwable {
                    Pulsar.getCurrent().rewind();
                    return SchemeUtils.NO_RESULT;
                }
            });
            
            PulsarDocuments.DOCS.defineDoc( env, new ProceduralDescriptiveBean(){{
                setNames( "rewind" );
                setParameterDescription( "" );
                setReturnValueDescription( "::void" );
                setShortDescription( "causes the music sequencer to go to the head of the song. " );
                setLongDescription( ""
                                    + "This procedure usually causes the music sequencer to call the main-procedure. "
                                    + "See (help set-main). "
                                    + THROWS_AN_ERROR_IF_NOT_OPEN );
            }} );
            
            Procedure simul = new MultipleNamedProcedureN( "simultaneous", "simul" ) {
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
            };
            SchemeUtils.defineLambda( env, simul );
            PulsarDocuments.DOCS.defineDoc( env, new ProceduralDescriptiveBean(){{
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
            }} );
            
            /////////////////////////////////////////////////////////////////
    
            Procedure getTrack = new MultipleNamedProcedureN( "get-track", "gett" ) {
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
            };
            SchemeUtils.defineLambda( env, getTrack );
            PulsarDocuments.DOCS.defineDoc( env, new ProceduralDescriptiveBean(){{
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
            }} );
            
            /////////////////////////////////////////////////////////////////
    
            PulsarDocuments.DOCS.defineDoc( env, new ProceduralDescriptiveBean(){{
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
            }} );
            
            /////////////////////////////////////////////////////////////////
            
            
            Procedure newTrack = new MultipleNamedProcedureN( "new-track", "newt" ) {
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
            };
            SchemeUtils.defineLambda( env, newTrack );
            PulsarDocuments.DOCS.defineDoc( env, new ProceduralDescriptiveBean(){{
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
            }} );
    
            
            
            Procedure newRecordingTrack = new MultipleNamedProcedureN( "new-recording-track", "rect" ) {
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
            };
            SchemeUtils.defineLambda( env, newRecordingTrack );
            PulsarDocuments.DOCS.defineDoc( env, new ProceduralDescriptiveBean(){{
                setNames( "new-recording-track" , "rect" );
                setParameterDescription( "[procedure/(list notation)]..." );
                addParameter( 0, "notations", "procedure/(list notation)", null, true, "The contents of the track. " );
                setReturnValueDescription( "::MetroTrack" );
                setShortDescription( "<name/> creates a new track." );
                setLongDescription( ""
                                    + "" 
                                    + THROWS_AN_ERROR_IF_NOT_OPEN );
            }} );
            
            
            
            
            
            
            /////////////////////////////////////////////////////////////////
            // ( canonical )
            PulsarDocuments.DOCS.defineDoc( env, new ProceduralDescriptiveBean(){{
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
            }} );
            
            // ( canonical )
            PulsarDocuments.DOCS.defineDoc( env, new ProceduralDescriptiveBean(){{
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
            }} );
    
            /////////////////////////////////////////////////////////////////
            
            abstract class TrackManagementProcedure extends MultipleNamedProcedureN  {
                TrackManagementProcedure( String ... names ) {
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
    
            /////////////////////////////////////////////////////////////////
    
            Procedure putTrack = new TrackManagementProcedure( "put-track" , "putt" ) {
                @Override
                void procTrack( List<MetroTrack> trackList, MetroSyncType syncType, MetroTrack syncTrack, double syncOffset ) {
                    Pulsar.getCurrent().putTrack(trackList, syncType, syncTrack, syncOffset);
                }
            };
            SchemeUtils.defineLambda( env, putTrack );
    
            ProceduralDescriptiveBean trackInitializer = new ProceduralDescriptiveBean(){{
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
            }};
    
            PulsarDocuments.DOCS.defineDoc( env, trackInitializer.processArguments( 
                "put",
                ""
                + "The sequencer starts to play the added track and it gives the user some controls on "
                + "how it starts playing the track."    
            ).setNames( "put-track", "putt" ));
    
            /////////////////////////////////////////////////////////////////
    
            Procedure removeTrack = new TrackManagementProcedure( "remove-track", "remt" ) {
                @Override
                void procTrack( List<MetroTrack> trackList, MetroSyncType syncType, MetroTrack syncTrack, double syncOffset ) {
                    Pulsar.getCurrent().removeTrack(trackList, syncType, syncTrack, syncOffset);
                }
            };
            SchemeUtils.defineLambda( env, removeTrack );
            PulsarDocuments.DOCS.defineDoc( env, trackInitializer.processArguments( 
                "removes",
                ""
                        + "The sequencer remove the specified track. Eventually the track stops playing. "
                        + "And it gives the user some controls on "
                        + "how it stops playing the track. "    
                    ).setNames( "remove-track", "remt" ) );
    
    
            Procedure notifyTrackChange = new MultipleNamedProcedure0( "notify-track-change", "nott" ) {
                @Override
                public Object apply0() throws Throwable {
                    Pulsar.getCurrent().notifyTrackChange();
                    return SchemeUtils.NO_RESULT;
                }
            };
            SchemeUtils.defineLambda( env, notifyTrackChange );
            PulsarDocuments.DOCS.defineDoc( env, new ProceduralDescriptiveBean(){{
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
            }} );
            
            SchemeUtils.defineLambda( env, new MultipleNamedProcedure0( "list-tracks", "lstt"  ) {
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
            });
    
            PulsarDocuments.DOCS.defineDoc( env, new ProceduralDescriptiveBean(){{
                setNames( "list-tracks", "lstt" );
                setParameterDescription( "" );
                setReturnValueDescription( "::(list track ...)" );
                setShortDescription( "||<name/>|| retrieves all tracks on the current sequencer. " );
                setLongDescription( ""
                                    + "The order of the tracks in the result of this procedure follows the first-in-last-out manner. "
                                    + "That is, (car (<name/>)) always returns the last added track. "
                                    + "" 
                                 );
            }} );       
            
            MultipleNamedProcedure0 clr = new MultipleNamedProcedure0( "clear-tracks", "clet" ) {
                @Override
                public Object apply0() throws Throwable {
                    Pulsar.getCurrent().clearTracks();
                    return SchemeUtils.NO_RESULT;
                }
            };
            SchemeUtils.defineLambda( env, clr );
            
            PulsarDocuments.DOCS.defineDoc( env, new ProceduralDescriptiveBean(){{
                setNames( "clear-tracks", "clet" );
                setParameterDescription( "" );
                setReturnValueDescription( "::void" );
                setShortDescription( "||<name/>|| removes all tracks on the current sequencer immediately. " );
                setLongDescription( ""
                                    + "" 
                                 );
            }} );   
    
            Procedure getmt = new MultipleNamedProcedure0( "get-main-track", "getmt" ) {
                @Override
                public Object apply0() throws Throwable {
                    return SchemeUtils.javaNullCheck( Pulsar.getCurrent().getMainTrack() );
                }
            };
            SchemeUtils.defineLambda( env, getmt );
            
            PulsarDocuments.DOCS.defineDoc( env, new ProceduralDescriptiveBean(){{
                setNames( "get-main-track", "getmt" );
                setParameterDescription( "" );
                setReturnValueDescription( "::void" );
                setShortDescription( "||<name/>|| retrieves the reference to the current main track." );
                setLongDescription( ""
                                  + "" 
                                 );
            }} );   
            
            Procedure gettp = new MultipleNamedProcedure1( "get-track-position", "gettp" ) {
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
            };
            SchemeUtils.defineLambda( env, gettp );
            
            PulsarDocuments.DOCS.defineDoc( env, new ProceduralDescriptiveBean(){{
                setNames( "get-track-position", "gettp" );
                setParameterDescription( "" );
                setReturnValueDescription( "::void" );
                setShortDescription( "||<name/>|| gets the current position of the given track." );
                setLongDescription( ""
                                    + "" 
                                 );
            }} );   
            
            
            
            
            
            
            
            SchemeUtils.defineLambda( env, new MultipleNamedProcedure0( "print-stack-trace" ) {
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
            });
            
            PulsarDocuments.DOCS.defineDoc( env, new ProceduralDescriptiveBean(){{
                setNames( "print-stack-trace" );
                setParameterDescription( "" );
                setReturnValueDescription( "::void" );
                setShortDescription( "||<name/>|| returns the current stack trace as a string. " );
                setLongDescription( ""
                                    + "" 
                                 );
            }} );   
    
            
            SchemeUtils.defineLambda( env, new MultipleNamedProcedure1( "display-warn" ) {
                @Override
                public Object apply1(Object arg1) throws Throwable {
                    System.err.print( arg1 );
                    return Values.empty;
                }
            });
    
            PulsarDocuments.DOCS.defineDoc( env, new ProceduralDescriptiveBean(){{
                setNames( "display-warn" );
                setParameterDescription( "any" );
                addParameter( 0, "value", "any", null, false , "" );
                setReturnValueDescription( "::void" );
                setShortDescription( "||<name/>|| output the specified value to the standard error stream. " );
                setLongDescription( ""
                                    + "" 
                                 );
            }} );
            
            SchemeUtils.defineLambda( env, new MultipleNamedProcedure0( "newline-warn" ) {
                @Override
                public Object apply0() throws Throwable {
                    System.err.println();
                    return Values.empty;
                }
            });
    
            PulsarDocuments.DOCS.defineDoc( env, new ProceduralDescriptiveBean(){{
                setNames( "newline-warn" );
                setParameterDescription( "" );
                setReturnValueDescription( "::void" );
                setShortDescription( "||<name/>|| output a line terminator to the standard error stream. " );
                setLongDescription( ""
                                    + "" 
                                 );
            }} );
            
            SchemeUtils.defineLambda( env, new MultipleNamedProcedureN( "typeof" ) {
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
            });
            
            PulsarDocuments.DOCS.defineDoc( env, new ProceduralDescriptiveBean(){{
                setNames( "typeof" );
                setParameterDescription( "any" );
                addParameter( 0, "value", "any", null, false , "" );
                setReturnValueDescription( "::string" );
                setShortDescription( "||<name/>|| returns a Java class name of the specified value. " );
                setLongDescription( "In case the specified value is a ||null|| of Java, this procedure returns \"null\" as a string value. "
                                    + "" 
                                 );
            }} );
            
    
            SchemeUtils.defineLambda( env, new MultipleNamedProcedureN( "schedule", "make-timer" ) {
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
                };
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
            });
    
            PulsarDocuments.DOCS.defineDoc( env, new ProceduralDescriptiveBean(){{
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
            }} );
            
            
    
            SchemeUtils.defineLambda( env, new MultipleNamedProcedure3( "add-event-listener" ) {
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
            } );
    
            PulsarDocuments.DOCS.defineDoc( env, new ProceduralDescriptiveBean(){{
                setNames( "add-event-listener" );
                addParameter( 0, "target",     "object",    null, false , "" );
                addParameter( 0, "event-type", "symbol",    null, false , "" );
                addParameter( 0, "callback",   "procedure", null, false , "" );
            
                setReturnValueDescription( "::void" );
                setShortDescription( "||<name/>|| registers the specified procedure as an event handler. " );
                setLongDescription( ""
                        + "" 
                );
            }} );
    
            SchemeUtils.defineLambda( env, new MultipleNamedProcedure2( "remove-event-listener" ) {
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
            } );
    
            PulsarDocuments.DOCS.defineDoc( env, new ProceduralDescriptiveBean() {{
                setNames( "remove-event-listener" );
                addParameter( 0, "target",     "object",    null, false , "" );
                addParameter( 0, "callback",   "procedure", null, false , "" );
            
                setReturnValueDescription( "::void" );
                setShortDescription( "||<name/>|| unregisters the specified procedure as an event handler. " );
                setLongDescription( ""
                        + "" 
                );
            }});
    
            
            SchemeUtils.defineLambda( env, new MultipleNamedProcedure1( "track?" ) {
                @Override
                public Object apply1(Object arg0 ) throws Throwable {
                    return arg0 instanceof MetroTrack;
                }
            });
    
            SchemeUtils.defineLambda( env, new MultipleNamedProcedure1( "track->procedure" ) {
                @Override
                public Object apply1(Object arg0 ) throws Throwable {
                    if ( arg0 instanceof MetroTrack ) {
                        return ((MetroTrack)arg0).getSequence();
                    } else {
                        throw new IllegalArgumentException( "the argument is not a track object." );
                    }
                }
            });
            
            SchemeUtils.defineLambda( env, new MultipleNamedProcedureN( "apply-track", "appt" ) {
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
            });
            
            SchemeUtils.defineLambda( env, new MultipleNamedProcedureN( "read-track", "reat" ) {
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
            });
    
            SchemeUtils.defineLambda( env, new MultipleNamedProcedureN( "create-process", "newp"  ) {
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
            });
    
            SchemeUtils.defineLambda( env, new MultipleNamedProcedureN( "destroy-process", "kilp" ) {
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
            }); 
            
            SchemeUtils.defineLambda( env, new MultipleNamedProcedureN( "kill-process", "fkilp" ) {
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
            });
    
            SchemeUtils.defineLambda( env, new MultipleNamedProcedure1( "sleep" ) {
                @Override
                public Object apply1(Object arg1) throws Throwable {
                    Thread.sleep( SchemeUtils.toInteger( arg1 ));
                    return Values.empty;
                }
            } );
            
            SchemeUtils.defineLambda( env, new MultipleNamedProcedureN( "random", "rnd" ) {
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
            });
            
            PulsarDocuments.DOCS.defineDoc( env, new ProceduralDescriptiveBean(){{
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
            }} );
            
            SchemeUtils.defineLambda( env, new MultipleNamedProcedureN( "luck" ) {
                @Override
                public Object applyN(Object[] args) throws Throwable {
                    double probability = args.length == 0 ? 0.5 : SchemeUtils.toDouble( args[0] );
                    if ( probability < 0 ) return false;
                    if ( 1.0<=probability  ) return true;
                    return Pulsar.getCurrent().random.nextBoolean( probability );
                }
    
            });
            
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
    
    
            PulsarDocuments.DOCS.defineDoc( env, new ProceduralDescriptiveBean(){{
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
            }} );
            
            PulsarDocuments.defineDoc( env, PulsarNoteListParser.getInstance() );
    
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
