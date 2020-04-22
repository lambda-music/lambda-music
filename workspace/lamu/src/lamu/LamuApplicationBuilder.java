package lamu;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.lang.invoke.MethodHandles;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Deque;
import java.util.List;
import java.util.logging.Level;

import kawapad.Kawapad;
import kawapad.KawapadFrame;
import lamu.lib.app.ApplicationComponent;
import lamu.lib.app.ApplicationVessel;
import lamu.lib.args.ArgsBuilder;
import lamu.lib.args.ArgsBuilderElement;
import lamu.lib.args.ArgsBuilderElementFactory;
import lamu.lib.args.ArgsBuilderStackKey;
import lamu.lib.args.ArgsCommandBuild;
import lamu.lib.args.ArgsCommandState;
import lamu.lib.args.ArgsNamedArgument;
import lamu.lib.args.ArgsQuotedStringSplitter;
import lamu.lib.evaluators.AsyncThreadManager;
import lamu.lib.evaluators.Evaluator;
import lamu.lib.evaluators.MultiplexEvaluator;
import lamu.lib.evaluators.RemoteEvaluator;
import lamu.lib.evaluators.SchemeEvaluator;
import lamu.lib.evaluators.StreamEvaluator;
import lamu.lib.evaluators.http.SchemeHttp;
import lamu.lib.evaluators.http.SchemeHttp.UserAuthentication;
import lamu.lib.evaluators.repl.ReplServer;
import lamu.lib.evaluators.repl.SimpleReplService;
import lamu.lib.helps.LamuAbstractDocument;
import lamu.lib.log.Logger;
import lamu.lib.stream.LoggerStream;
import lamu.lib.stream.NullOutputStream;
import lamu.lib.stream.NullStream;
import lamu.lib.stream.SisoReceiver;
import lamu.lib.stream.StdioStream;
import lamu.lib.stream.Stream;
import pulsar.Pulsar;
import pulsar.PulsarFrame;

public class LamuApplicationBuilder extends ArgsCommandBuild {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }

    static final String MSG_NO_STREAM_ERROR     = "no stream is available";
    static final String MSG_NO_LANG_ERROR  = "no evaluator was specified";
    static final String MSG_OUTPUT_HELP_ERROR   = "an error occured in output-help";
    static final String MSG_UNKNOWN_PARAM_ERROR = "unknown parameter : ";
    
    // basic
    static final ArgsBuilderStackKey<Runnable> RUNNABLE_INIT  = new ArgsBuilderStackKey<>();
    static final ArgsBuilderStackKey<Runnable> RUNNABLE_START = new ArgsBuilderStackKey<>();
    static final ArgsBuilderStackKey<ApplicationVessel> VESSELS = new ArgsBuilderStackKey<>();
    static final ArgsBuilderStackKey<Stream>   STREAMABLES = new ArgsBuilderStackKey<>();

    // application
    static final ArgsBuilderStackKey<MultiplexEvaluator> EVALUATOR = new ArgsBuilderStackKey<>();
    static final ArgsBuilderStackKey<Pulsar> PULSAR = new ArgsBuilderStackKey<>();
    static final ArgsBuilderStackKey<Kawapad> KAWAPAD = new ArgsBuilderStackKey<>();
    static final ArgsBuilderStackKey<SisoReceiver> REPL = new ArgsBuilderStackKey<>();
    static final ArgsBuilderStackKey<KawapadFrame> FRAME = new ArgsBuilderStackKey<>();
    static final ArgsBuilderStackKey<SchemeHttp> HTTP = new ArgsBuilderStackKey<>();

    static Stream vessel2stream( ApplicationVessel vessel ) {
        return (Stream)vessel.getComponents()
            .stream()
            .filter( item-> item instanceof Stream )
            .findAny()
            .orElse(null);
    }

    static final class AllAvailableReferenceArgumentParserElementFactory implements ArgsBuilderElementFactory {
        @Override
        public ArgsBuilderElement create() {
            return new ArgsBuilderElement() {
                String outputFile = null;
                @Override
                public ArgsBuilderElement notifyArg(ArgsBuilder parser, String s) {
                    if ( s.startsWith( "--" ) ) {
                        ArgsNamedArgument narg = new ArgsNamedArgument( s );
                        switch ( narg.getKey() ) {
                        case "output-file" :
                            outputFile = narg.getValue();
                            break;
                        default :
                            throw new RuntimeException( MSG_UNKNOWN_PARAM_ERROR + narg.getKey() );
                        }
                    } else {
                        throw new RuntimeException( MSG_UNKNOWN_PARAM_ERROR + s );
                    }
                    return this;
                }
                @Override
                public void notifyEnd( ArgsBuilder parser ) {
                    if ( parser.getValueStack( EVALUATOR ).isEmpty() ) {
                        throw new RuntimeException( MSG_NO_LANG_ERROR );
                    }
                    LamuApplication.loadBasicClasses();
                    parser.getValueStack( RUNNABLE_INIT ).add( new Runnable() {
                        @Override
                        public void run() {
                            try {
                                LamuAbstractDocument.outputAvailableReferences( outputFile );
                            } catch (IOException e) {
                                logError( MSG_OUTPUT_HELP_ERROR, e ); 
                            }
                        }
                    });
                }
            };
        }
    }

    static final class OutputReferenceArgumentParserElementFactory implements ArgsBuilderElementFactory {
        @Override
        public ArgsBuilderElement create() {
            return new ArgsBuilderElement() {
                String outputFile = null;
                String category   = null;
                @Override
                public ArgsBuilderElement notifyArg(ArgsBuilder parser, String s) {
                    if ( s.startsWith( "--" ) ) {
                        ArgsNamedArgument narg = new ArgsNamedArgument( s );
                        switch ( narg.getKey() ) {
                        case "category" :
                            category = narg.getValue();
                            break;
                        case "output-file" :
                            outputFile = narg.getValue();
                            break;
                        default :
                            throw new RuntimeException( MSG_UNKNOWN_PARAM_ERROR  + narg.getKey() );
                        }
                    } else {
                        throw new RuntimeException( MSG_UNKNOWN_PARAM_ERROR  + s );
                    }
                    return this;
                }
                @Override
                public void notifyEnd(ArgsBuilder parser) {
                    /*
                     * (Sat, 07 Mar 2020 20:23:21 +0900)
                     * ### SPECIAL ###  
                     * Check for special keyword(s) and process differently that cannot be processed
                     * by the `Descriptive` Documentation System. 
                     * 
                     */
                    switch ( this.category ) {
                    case "kawapad-keystrokes" :
                        /*
                         *  This is a special keyword to output keystroke reference which is 
                         *  not processed in `Desctiptive` help system.
                         */
                        procKeyStroke(parser);
                        break;
                        /*
                         * The others are processed by the documentation system. 
                         */
                    default :
                        procDocument(parser);
                        break;
                    }
                }
                void procKeyStroke(ArgsBuilder parser) {
                    if ( parser.getValueStack( EVALUATOR ).isEmpty() ) {
                        throw new RuntimeException( MSG_NO_LANG_ERROR );
                    }
                    MultiplexEvaluator multiplexEvaluator = parser.getValueStack( EVALUATOR ).peek();

                    LamuApplication.loadBasicClasses();
                    parser.getValueStack( RUNNABLE_INIT ).add( new Runnable() {
                        @Override
                        public void run() {
                            Kawapad kawapad = new Kawapad(multiplexEvaluator);
                            String s = kawapad.outputKeyStrokeReference();
                            FileOutputStream o=null;
                            try {
                                o = new FileOutputStream( new File( outputFile ) );
                                o.write( s.getBytes(Charset.forName("utf-8")));
                                o.flush();
                            } catch (IOException e) {
                                logError( MSG_OUTPUT_HELP_ERROR, e ); 
                            } finally {
                                try {
                                    o.close();
                                } catch (IOException e) {
                                    e.printStackTrace();
                                }
                            }
                        }
                    });


                }
                void procDocument(ArgsBuilder parser) {
                    if ( parser.getValueStack( EVALUATOR ).isEmpty() ) {
                        throw new RuntimeException( MSG_NO_LANG_ERROR );
                    }
                    LamuApplication.loadBasicClasses();
                    parser.getValueStack( RUNNABLE_INIT ).add( new Runnable() {
                        @Override
                        public void run() {
                            try {
                                LamuAbstractDocument.outputReference( category, outputFile ); 
                            } catch (IOException e) {
                                logError( MSG_OUTPUT_HELP_ERROR, e ); 
                            }
                        }
                    });
                }
            };
        }
    }

    static final class SimpleReplArgumentParserElementFactory implements ArgsBuilderElementFactory {
        @Override
        public ArgsBuilderElement create() {
            return new ArgsBuilderElement() {
                //                int port = 0;
                @Override
                public ArgsBuilderElement notifyArg( ArgsBuilder parser, String s ) {
                    if ( false ) {
                    } else {
                        throw new RuntimeException( MSG_UNKNOWN_PARAM_ERROR + s );
                    }
                    return this;
                }
                @Override
                public void notifyEnd(ArgsBuilder parser) {
                    if ( parser.getValueStack( STREAMABLES ).isEmpty() ) {
                        throw new RuntimeException( MSG_NO_STREAM_ERROR );
                    }
                    Stream stream = parser.getValueStack( STREAMABLES ).peek();
                    SisoReceiver sisoReceiver = new SisoReceiver( stream, new SimpleReplService() );
                    parser.getValueStack( REPL ).push( sisoReceiver );
                }
            };
        }
    }

    static final class SchemeHttpServerArgumentParserElementFactory implements ArgsBuilderElementFactory {
        @Override
        public ArgsBuilderElement create() {
            return new ArgsBuilderElement() {
                private int serverPort = 8192;
                // TODO 0 variable accept path for HTTP 
                private String serverPath = "";
                UserAuthentication userAuthentication = UserAuthentication.ONLY_LOOPBACK;
                @Override
                public ArgsBuilderElement notifyArg(ArgsBuilder parser, String s) {
                    if (s.startsWith( "--" ) ) {
                        ArgsNamedArgument a = new ArgsNamedArgument(s);
                        switch ( a.getKey() ) {
                        case "port" : 
                            this.serverPort = Integer.parseInt( a.getValue() );
                            break;
                        case "path" : 
                            this.serverPath = a.getValue();
                            break;
                        case "auth" : 
                            if ( "only-loopback".equals( a.getValue() )  ) {
                                this.userAuthentication = UserAuthentication.ONLY_LOOPBACK;
                                // } else if ( "only-loopback".equals( a.value )  ) {
                                // TODO
                            } else {
                                throw new RuntimeException( "unknown authentication type : " + a.getKey() );
                            }
                            break;
                        default :
                            throw new RuntimeException( MSG_UNKNOWN_PARAM_ERROR + a.getKey() );
                        }
                    } else {
                        throw new RuntimeException( MSG_UNKNOWN_PARAM_ERROR + s );
                    }
                    return this;
                }
                @Override
                public void notifyEnd(ArgsBuilder parser) {
                    if ( parser.getValueStack( EVALUATOR ).isEmpty() ) {
                        throw new RuntimeException( MSG_NO_LANG_ERROR );
                    }
                    MultiplexEvaluator multiplexEvaluator = parser.getValueStack( EVALUATOR ).peek();
                    //                        if ( pulsarStack.isEmpty() ) {
                    //                            throw new RuntimeException( "no pulsar is defined." );
                    //                        }
                    //                        Pulsar pulsar = pulsarStack.peek();

                    try {
                        SchemeHttp schemeHttp = LamuApplicationLibrary.createPulsarHttpServer( 
                                multiplexEvaluator, serverPort, serverPath, userAuthentication );
                        parser.getValueStack( HTTP ).push( schemeHttp );
                    } catch (IOException e) {
                        throw new RuntimeException( e );
                    }
                }
            };
        }
    }

    static final class PulsarGuiArgumentParserElementFactory implements ArgsBuilderElementFactory {
        @Override
        public ArgsBuilderElement create() {
            return new ArgsBuilderElement() {
                List<String> fileNameList = new ArrayList<>();
                @Override
                public ArgsBuilderElement notifyArg(ArgsBuilder parser, String s) {
                    if ( s.startsWith( "--" ) ) {
                        ArgsNamedArgument a = new ArgsNamedArgument(s);
                        switch ( a.getKey() ) {
                        case "open" : 
                        case "load" : 
                            fileNameList.add( a.getValue() );
                            break;
                        default :
                            throw new RuntimeException( MSG_UNKNOWN_PARAM_ERROR + a.getKey() );
                        }
                    } else {
                        fileNameList.add(s);
                    }
                    return this;
                }
                @Override
                public void notifyEnd(ArgsBuilder parser) {
                    if ( parser.getValueStack( EVALUATOR ).isEmpty() ) {
                        throw new RuntimeException( MSG_NO_LANG_ERROR );
                    }
                    MultiplexEvaluator multiplexEvaluator = parser.getValueStack( EVALUATOR ).peek();

                    PulsarFrame frame = LamuApplicationLibrary.createPulsarGui( multiplexEvaluator );
                    if (parser.getValueStack( FRAME ).size() == 0 )
                        frame.setShutdownWhenClose( true );
                    else
                        frame.setShutdownWhenClose( false );

                    parser.getValueStack( FRAME ).push( frame );
                    parser.getValueStack( KAWAPAD ).push( frame.getKawapad() );

                    parser.getValueStack( RUNNABLE_INIT ).push( new KawapadLoader( frame, fileNameList ));
                }
            };
        }
    }

    static final class KawapadGuiArgumentParserElementFactory implements ArgsBuilderElementFactory {
        @Override
        public ArgsBuilderElement create() {
            List<String> fileNameList = new ArrayList<>();
            return new ArgsBuilderElement() {
                @Override
                public ArgsBuilderElement notifyArg(ArgsBuilder parser, String s) {
                    if ( s.startsWith( "--" ) ) {
                        ArgsNamedArgument a = new ArgsNamedArgument(s);
                        switch ( a.getKey() ) {
                        //                                case "port" : 
                        //                                    portNumberList.add(  Integer.parseInt( a.value ) );
                        //                                    break;
                        default :
                            throw new RuntimeException( MSG_UNKNOWN_PARAM_ERROR + a.getKey() );
                        }
                    } else {
                        fileNameList.add(s);
                    }
                    return this;
                }
                @Override
                public void notifyEnd(ArgsBuilder parser) {
                    if ( parser.getValueStack( EVALUATOR ).isEmpty() ) {
                        throw new RuntimeException( MSG_NO_LANG_ERROR );
                    }
                    MultiplexEvaluator multiplexEvaluator = parser.getValueStack( EVALUATOR ).peek();
                    KawapadFrame frame = LamuApplicationLibrary.createKawapadGui( multiplexEvaluator );

                    if (parser.getValueStack( FRAME ).size() == 0 )
                        frame.setShutdownWhenClose( true );
                    else
                        frame.setShutdownWhenClose( false );

                    parser.getValueStack( FRAME ).push( frame );
                    parser.getValueStack( KAWAPAD ).push( frame.getKawapad() );
                    parser.getValueStack( RUNNABLE_INIT ).push( new KawapadLoader( frame, fileNameList ));
                    parser.getValueStack( RUNNABLE_INIT ).push( new Runnable() {
                        @Override
                        public void run() {
                            //                                pulsar.init();
                        }
                    });
                }
            };
        }
    }

    static final class PulsarArgumentParserElementFactory implements ArgsBuilderElementFactory {
        @Override
        public ArgsBuilderElement create() {
            return new ArgsBuilderElement() {
                @Override
                public ArgsBuilderElement notifyArg(ArgsBuilder parser, String s) {
                    throw new RuntimeException( MSG_UNKNOWN_PARAM_ERROR + s);
                }
                @Override
                public void notifyEnd(ArgsBuilder parser) {
                    Pulsar pulsar = LamuApplicationLibrary.createPulsar();
                    parser.getValueStack( PULSAR ).push( pulsar );
                    parser.getValueStack( RUNNABLE_INIT ).push( new Runnable() {
                        @Override
                        public void run() {
                        }
                    });
                }
            };
        }
    }
    static final class ReplArgumentParserElementFactory implements ArgsBuilderElementFactory {
        @Override
        public ArgsBuilderElement create() {
            return new ArgsBuilderElement() {
                List<String> fileNameList = new ArrayList<>();
                List<String> scriptStringList = new ArrayList<>();
                @Override
                public ArgsBuilderElement notifyArg(ArgsBuilder parser, String s) {
                    if ( s.startsWith( "--" ) ) {
                        ArgsNamedArgument a = new ArgsNamedArgument(s);
                        switch ( a.getKey() ) {
                        case "load" : 
                            fileNameList.add( a.getValue() );
                            break;
                        default :
                            throw new RuntimeException( MSG_UNKNOWN_PARAM_ERROR + a.getKey() );
                        }
                    } else {
                        fileNameList.add(s);
                    }
                    return this;
                }
                @Override
                public void notifyEnd(ArgsBuilder parser) {
                    if ( parser.getValueStack( EVALUATOR ).isEmpty() ) {
                        throw new RuntimeException( MSG_NO_LANG_ERROR );
                    }
                    MultiplexEvaluator multiplexEvaluator = parser.getValueStack( EVALUATOR ).peek();

                    if ( parser.getValueStack( STREAMABLES ).isEmpty() ) {
                        throw new RuntimeException( MSG_NO_STREAM_ERROR );
                    }
                    Stream stream = parser.getValueStack( STREAMABLES ).peek();

                    
                    ReplServer replServer = new ReplServer( new AsyncThreadManager(), multiplexEvaluator );
                    SisoReceiver<ReplServer> receiver = new SisoReceiver<>( stream, replServer );
                    parser.getValueStack( REPL ).push( receiver );
                    parser.getValueStack( RUNNABLE_INIT ).push( new Runnable() {
                        boolean done = false;
                        @Override
                        public void run() {
                            if ( ! done ) {
                                done=true;
                                for ( String fileName : fileNameList ) {
                                    try {
                                        scriptStringList.add( "; " );
                                        scriptStringList.add( "; " + fileName );
                                        scriptStringList.add( "; " );
                                        scriptStringList.addAll( ArgsQuotedStringSplitter.splitLines( Utils.readAllAsString( fileName )));
                                        scriptStringList.add( " " );
                                    } catch (IOException e) {
                                        throw new Error(e);
                                    }
                                }
                                scriptStringList.add( replServer.getPrefix() + "exec" );
                            } else {
                                throw new Error( "duplicate calling" );
                            }
                        }
                    });
                    parser.getValueStack( RUNNABLE_START ).push( new Runnable() {
                        @Override
                        public void run() {
                            receiver.process( scriptStringList );
                        }
                    });
                }
            };
        }
    }
    static final class SchemeArgumentParserElementFactory implements ArgsBuilderElementFactory {
        static final String DEFAULT_REMOTE_URL = "http://localhost:";
        @Override
        public ArgsBuilderElement create() {
            return new ArgsBuilderElement() {
                MultiplexEvaluator multiplexEvaluator = MultiplexEvaluator.createEmpty();
                
                Evaluator createRemoteHttp( String url ) {
                    return new RemoteEvaluator( url );
                }
                Evaluator createRemoteStream( Stream streamable ) {
                    return new StreamEvaluator( streamable );
                }
                
                List<Evaluator> evaluatorList = new ArrayList<>();
                @Override
                public ArgsBuilderElement notifyArg(ArgsBuilder parser, String s) {
                    if ( s.startsWith( "--"  ) ) {
                        ArgsNamedArgument a = new ArgsNamedArgument( s );
                        if ( false ) {
                            //
                        } else if ( "local".equals( a.getKey() ) ) {
                            evaluatorList.add( new SchemeEvaluator() );
                        } else if ( "server-url".equals( a.getKey() ) ) {
                            // deprecated
                            evaluatorList.add( createRemoteHttp( a.getValue() ) );
                        } else if ( "server-port".equals( a.getKey() ) ) {
                            // deprecated
                            evaluatorList.add( createRemoteHttp( DEFAULT_REMOTE_URL + a.getValue() ) );
                        } else if ( "http".equals( a.getKey() ) ) {
                            evaluatorList.add( createRemoteHttp( a.getValue() ) );
                        } else if ( "stream".equals( a.getKey() ) ) {
                            evaluatorList.add( createRemoteStream( parser.getValueStack( STREAMABLES ).peek()));
                        } else {
                            throw new RuntimeException( "unknown argument " + s );
                        }
                    } else {
                        throw new RuntimeException( "unknown command " + s );
                    }
                    return this;
                }
                @Override
                public void notifyEnd(ArgsBuilder parser) {
                    if (this.evaluatorList.isEmpty() ) {
                        this.evaluatorList.add( new SchemeEvaluator() );
                    }
                    this.multiplexEvaluator.addAllEvaluators( this.evaluatorList );
                    parser.getValueStack( EVALUATOR ).push( this.multiplexEvaluator );

                    //                        runnableStack.push( new Runnable() {
                    //                            @Override
                    //                            public void run() {
                    //                                schemeSecretary.newScheme();
                    //                            }
                    //                        });
                }
            };
        }
    }

    static final class KawapadLoader implements Runnable {
        private final KawapadFrame frame;
        private List<String> fileNameList;
        KawapadLoader(KawapadFrame pulsarFrame, List<String> fileNameList) {
            this.frame = pulsarFrame;
            this.fileNameList = fileNameList;
        }

        @Override
        public void run() {
            frame.processInit();
            boolean first=true;
            for ( String s : fileNameList ) {
                try {
                    if ( first ) { 
                        frame.getKawapad().openFile( new File(s) );
                    } else {
                        frame.getKawapad().createKawapadFrame( new File(s) );
                    }
                } catch (IOException e) {
                    e.printStackTrace();
                }
                first=false;
            }
            if ( first ) {
                frame.getKawapad().openIntro();
            }
        }
    }
    
    static final class LoggerArgumentParserElementFactory implements ArgsBuilderElementFactory {
        @Override
        public ArgsBuilderElement create() {
            return new ArgsBuilderElement() {
                private String outFile;
                private String errFile;
                private String inFile;
                @Override
                public ArgsBuilderElement notifyArg(ArgsBuilder parser, String s) {
                    if ( s.startsWith( "--" ) ) {
                        ArgsNamedArgument narg = new ArgsNamedArgument( s );
                        switch ( narg.getKey() ) {
                        case "out" :
                            outFile = narg.getValue();
                            break;
                        case "err" :
                            errFile = narg.getValue();
                            break;
                        case "in" :
                            inFile = narg.getValue();
                            break;
                        default :
                            throw new RuntimeException( MSG_UNKNOWN_PARAM_ERROR + narg.getKey() );
                        }
                    } else {
                        throw new RuntimeException( MSG_UNKNOWN_PARAM_ERROR + s );
                    }
                    return this;
                }
                @Override
                public void notifyEnd( ArgsBuilder parser ) {
                    if ( parser.getValueStack( STREAMABLES ).isEmpty() ) {
                        throw new RuntimeException( MSG_NO_STREAM_ERROR );
                    }
                    Stream streamable = parser.getValueStack( STREAMABLES ).peek();
                    
                    try {
                        parser.getValueStack( STREAMABLES ).push(
                            new LoggerStream( streamable,
                                ( inFile  == null ? new NullOutputStream() : new FileOutputStream( inFile  )),
                                ( outFile == null ? new NullOutputStream() : new FileOutputStream( outFile )),
                                ( errFile == null ? new NullOutputStream() : new FileOutputStream( errFile ))));
                    } catch ( FileNotFoundException e ) {
                        throw new Error(e);
                    } 
                }
            };
        }

    }
    static final class StdioArgumentParserElementFactory implements ArgsBuilderElementFactory {
        @Override
        public ArgsBuilderElement create() {
            return new ArgsBuilderElement() {
                @Override
                public ArgsBuilderElement notifyArg(ArgsBuilder parser, String s) {
                    return this;
                }
                @Override
                public void notifyEnd( ArgsBuilder parser ) {
                    initStream(parser);
                }

                void initStream(ArgsBuilder parser) {
                    parser.getValueStack( STREAMABLES ).push( StdioStream.INSTANCE );
                }

//                /**
//                 * Initialize the default stream. See {@link lamu.lib.app.args.ArgsState#initStream() }
//                 */
//                void initStream(ArgumentParser parser) {
//                    if ( ConsoleChecker.consoleExists() ) {
//                        parser.getValueStack( STREAMABLES ).push( StdioStream.INSTANCE );
//                    } else {
//                        logWarn(
//                            "=== WARNING ===\n" +
//                                "The user requested a STDIO stream to be instantiated but it is not available;\n" +
//                            "therefore, a dummy stream is created instead of the STDIO.\n" );
//                        parser.getValueStack( STREAMABLES ).push( NullStream.INSTANCE );
//                    }
//                }
            };
        }
    }
    static final class NullStreamArgumentParserElementFactory implements ArgsBuilderElementFactory {
        @Override
        public ArgsBuilderElement create() {
            return new ArgsBuilderElement() {
                @Override
                public ArgsBuilderElement notifyArg(ArgsBuilder parser, String s) {
                    return this;
                }
                @Override
                public void notifyEnd( ArgsBuilder parser ) {
                    parser.getValueStack( STREAMABLES ).push( NullStream.INSTANCE );
                }
            };
        }
    }
    static final class ForkedArgumentParserElementFactory implements ArgsBuilderElementFactory {
        @Override
        public ArgsBuilderElement create() {
            return new ArgsBuilderElement() {
                @Override
                public ArgsBuilderElement notifyArg(ArgsBuilder parser, String s) {
                    return this;
                }
                @Override
                public void notifyEnd( ArgsBuilder parser ) {
                    ApplicationVessel vessel = parser.getValueStack( VESSELS ).peek();
                    if ( vessel == null ) {
                        throw new Error( "no vessel was found" );
                    }
                    Stream stream = vessel2stream(vessel);
                    if ( stream == null ) {
                        throw new Error( "no stream was found" );
                    }
                    parser.getValueStack( STREAMABLES ).push( stream );
                }
            };
        }
    }


    /**
     * Sets up the commands for the command-line parser. Please read the code of the 
     * {@link LamuApplicationBuilder#initializeParser()} 
     * <pre>{@code
     * registerFactory("scheme",           new SchemeArgumentParserElementFactory());
     * registerFactory("kawapad",          new KawapadGuiArgumentParserElementFactory());
     * registerFactory("pulsar",           new PulsarArgumentParserElementFactory());
     * registerFactory("gui",              new PulsarGuiArgumentParserElementFactory());
     * registerFactory("http",            new SchemeServerArgumentParserElementFactory());
     * registerFactory("output-help",      new OutputReferenceArgumentParserElementFactory());
     * registerFactory("output-help-list", new AllAvailableReferenceArgumentParserElementFactory());
     * }</pre>
     * @return 
     */
    static void initializeBuilder0( ArgsBuilder builder ) {
        builder.registerFactory( "scheme",           new SchemeArgumentParserElementFactory());
        builder.registerFactory( "kawapad",          new KawapadGuiArgumentParserElementFactory());
        builder.registerFactory( "pulsar",           new PulsarArgumentParserElementFactory());
        builder.registerFactory( "repl",             new ReplArgumentParserElementFactory());
        builder.registerFactory( "gui",              new PulsarGuiArgumentParserElementFactory());
        builder.registerFactory( "http",             new SchemeHttpServerArgumentParserElementFactory());
        builder.registerFactory( "simple-repl",      new SimpleReplArgumentParserElementFactory());
        builder.registerFactory( "logger-stream",    new LoggerArgumentParserElementFactory());
        builder.registerFactory( "stdio-stream",     new StdioArgumentParserElementFactory());
        builder.registerFactory( "null-stream",      new NullStreamArgumentParserElementFactory());
        builder.registerFactory( "forked-stream",    new ForkedArgumentParserElementFactory());
        
        builder.registerFactory( "reference",        new OutputReferenceArgumentParserElementFactory());
        builder.registerFactory( "reference-list",   new AllAvailableReferenceArgumentParserElementFactory());
    }
    
    static void finalizeBuilder0( ArgsCommandState state, ArgsBuilder builder) {
        // Collect all components
        ArrayList<Object> allObjects = new ArrayList<>();
        ArrayList<ApplicationComponent> allComponents = new ArrayList<>();

        {
            ArrayList<Deque> stackList2 = new ArrayList<>( builder.getValueStackList() );
            
            // The vessels are not necessary here; therefore exclude it from the list.
            stackList2.remove( builder.getValueStack( VESSELS ) );
            stackList2.remove( builder.getValueStack( STREAMABLES ) );
            
            logInfo( "deploy:==== COLLECT COMPONENTS ==============================" );
            for ( Deque stack : stackList2 ) {
                for ( Object o : stack ) {
                    logInfo( "deploy:"  + o.toString() );
                    
                    // 1.
                    allObjects.add(o);
                    
                    // 2.
                    if ( o instanceof ApplicationComponent ) {
                        allComponents.add((ApplicationComponent) o );
                    }
                }
            }
            logInfo( "deploy:======================================================" );
        }

        // Initialize ThreadInitializer
        ApplicationVessel vessel = new ApplicationVessel( "ExecVessel" );

        // Collect all application compnents.
        vessel.addAll( allComponents );
        
        // Executing runnable(init) stack;
        {
            ArrayList<Runnable> list = new ArrayList<Runnable>( builder.getValueStack( RUNNABLE_INIT ) );
            Collections.reverse( list );
            vessel.add( new ArgsBuilder.Initializer( list ) );
        }

        // Executing runnable(start) stack;
        {
            ArrayList<Runnable> list = new ArrayList<Runnable>( builder.getValueStack( RUNNABLE_START ) );
            Collections.reverse( list );
            vessel.add( new ArgsBuilder.Initializer( list ) );
        }
 
        builder.getValueStack( VESSELS ).push( vessel );
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    
    static <T> void setCollection( Collection<T> from, Collection<T> to ) {
        to.clear();
        to.addAll(from);
    }
    @Override
    protected void initializeBuilder(ArgsCommandState state0, ArgsBuilder builder) {
        LamuCommandState state = (LamuCommandState) state0;
        initializeBuilder0( builder );
        setCollection( state.vessels,     builder.getValueStack( VESSELS ) );
        setCollection( state.streamables, builder.getValueStack( STREAMABLES ) );
    }
    @Override
    protected void finalizeBuilder(ArgsCommandState state0, ArgsBuilder builder) {
        LamuCommandState state = (LamuCommandState) state0;
        finalizeBuilder0( state, builder);
        setCollection( builder.getValueStack( VESSELS ),     state.vessels  );
        setCollection( builder.getValueStack( STREAMABLES ), state.streamables );
    }
}
