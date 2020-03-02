package lamu;

import java.io.File;
import java.io.IOException;
import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;

import kawapad.Kawapad;
import kawapad.KawapadFrame;
import pulsar.Pulsar;
import pulsar.PulsarFrame;
import quartz.lib.app.args.ArgumentParser;
import quartz.lib.app.args.ArgumentParserDefault;
import quartz.lib.app.args.ArgumentParserElement;
import quartz.lib.app.args.ArgumentParserElementFactory;
import quartz.lib.app.args.ArgumentParserStackKey;
import quartz.lib.log.SimpleConsoleLogger;
import quartz.lib.scheme.EvaluatorManager;
import quartz.lib.scheme.SchemeEngine;
import quartz.lib.scheme.doc.DescriptiveDocumentCategory;
import quartz.lib.scheme.http.SchemeHttp;
import quartz.lib.scheme.http.SchemeHttp.UserAuthentication;

class LamuApplicationArgumentParser extends ArgumentParserDefault {
    static final SimpleConsoleLogger LOGGER = SimpleConsoleLogger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }

    @Override
    protected void createValueStackMap(){
        getValueStack( SCHEME_ENGINE );
        getValueStack( PULSAR );
        getValueStack( KAWAPAD );
        getValueStack( FRAME );
        getValueStack( SCHEME_HTTP );
        getValueStack( RUNNABLE );
    }

    static final ArgumentParserStackKey<SchemeEngine> SCHEME_ENGINE = new ArgumentParserStackKey<>();
    static final ArgumentParserStackKey<Pulsar> PULSAR = new ArgumentParserStackKey<>();
    static final ArgumentParserStackKey<Kawapad> KAWAPAD = new ArgumentParserStackKey<>();
    static final ArgumentParserStackKey<KawapadFrame> FRAME = new ArgumentParserStackKey<>();
    static final ArgumentParserStackKey<SchemeHttp> SCHEME_HTTP= new ArgumentParserStackKey<>();
    
    static final class AllAvailableReferenceArgumentParserElementFactory implements ArgumentParserElementFactory {
        @Override
        public ArgumentParserElement create() {
            return new ArgumentParserElement() {
                String outputFile = null;
                @Override
                public ArgumentParserElement notifyArg(ArgumentParser parser, String s) {
                    if ( s.startsWith( "--" ) ) {
                        LamuApplicationNamedArgument narg = new LamuApplicationNamedArgument( s );
                        switch ( narg.getKey() ) {
                            case "output-file" :
                                outputFile = narg.getValue();
                                break;
                            default :
                                throw new RuntimeException( "unknown parameter : " + narg.getKey() );
                        }
                    } else {
                        throw new RuntimeException( "unknown parameter : " + s );
                    }
                    return this;
                }
                @Override
                public void notifyEnd( ArgumentParser parser ) {
                    if ( parser.getValueStack( SCHEME_ENGINE ).isEmpty() ) {
                        throw new RuntimeException( "no scheme is defined." );
                    }
                    LamuApplication.loadBasicClasses();
                    parser.getValueStack( RUNNABLE ).add( new Runnable() {
                        @Override
                        public void run() {
                            try {
                                DescriptiveDocumentCategory.outputAvailableReferences( outputFile );
                            } catch (IOException e) {
                                logError( "error occured in output-reference", e ); 
                            }
                        }
                    });
                }
            };
        }
    }

    static final class OutputReferenceArgumentParserElementFactory implements ArgumentParserElementFactory {
        @Override
        public ArgumentParserElement create() {
            return new ArgumentParserElement() {
                String outputFile = null;
                String category   = null;
                @Override
                public ArgumentParserElement notifyArg(ArgumentParser parser, String s) {
                    if ( s.startsWith( "--" ) ) {
                        LamuApplicationNamedArgument narg = new LamuApplicationNamedArgument( s );
                        switch ( narg.getKey() ) {
                            case "category" :
                                category = narg.getValue();
                                break;
                            case "output-file" :
                                outputFile = narg.getValue();
                                break;
                            default :
                                throw new RuntimeException( "unknown parameter : " + narg.getKey() );
                        }
                    } else {
                        throw new RuntimeException( "unknown parameter : " + s );
                    }
                    return this;
                }
                @Override
                public void notifyEnd(ArgumentParser parser) {
                    if ( parser.getValueStack( SCHEME_ENGINE ).isEmpty() ) {
                        throw new RuntimeException( "no scheme is defined." );
                    }
                    SchemeEngine schemeEngine = parser.getValueStack( SCHEME_ENGINE ).peek();
                    LamuApplication.loadBasicClasses();
                    parser.getValueStack( RUNNABLE ).add( new Runnable() {
                        @Override
                        public void run() {
                            try {
                                DescriptiveDocumentCategory.outputReference( 
                                    schemeEngine.getSchemeEvaluator().getScheme().getEnvironment(), 
                                    category, outputFile );
                            } catch (IOException e) {
                                logError( "error occured in output-reference", e ); 
                            }
                        }
                    });
                }
            };
        }
    }

    static final class SchemeServerArgumentParserElementFactory implements ArgumentParserElementFactory {
        @Override
        public ArgumentParserElement create() {
            return new ArgumentParserElement() {
                private int serverPort = 8192;
                // TODO 0 variable accept path for HTTP 
                private String serverPath = "";
                UserAuthentication userAuthentication = UserAuthentication.ONLY_LOOPBACK;
                @Override
                public ArgumentParserElement notifyArg(ArgumentParser parser, String s) {
                    if (s.startsWith( "--" ) ) {
                        LamuApplicationNamedArgument a = new LamuApplicationNamedArgument(s);
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
                                throw new RuntimeException( "unknown parameter : " + a.getKey() );
                        }
                    } else {
                        throw new RuntimeException( "unknown parameter : " + s );
                    }
                    return this;
                }
                @Override
                public void notifyEnd(ArgumentParser parser) {
                    if ( parser.getValueStack( SCHEME_ENGINE ).isEmpty() ) {
                        throw new RuntimeException( "no scheme is defined." );
                    }
                    SchemeEngine schemeEngine = parser.getValueStack( SCHEME_ENGINE ).peek();
//                        if ( pulsarStack.isEmpty() ) {
//                            throw new RuntimeException( "no pulsar is defined." );
//                        }
//                        Pulsar pulsar = pulsarStack.peek();
                    
                    try {
                        SchemeHttp schemeHttp = LamuApplicationLibrary.createPulsarHttpServer( 
                            schemeEngine, serverPort, serverPath, userAuthentication );
                        parser.getValueStack( SCHEME_HTTP ).push( schemeHttp );
                    } catch (IOException e) {
                        throw new RuntimeException( e );
                    }
                }
            };
        }
    }

    static final class PulsarGuiArgumentParserElementFactory implements ArgumentParserElementFactory {
        @Override
        public ArgumentParserElement create() {
            return new ArgumentParserElement() {
                List<String> fileNameList = new ArrayList<>();
                @Override
                public ArgumentParserElement notifyArg(ArgumentParser parser, String s) {
                    if ( s.startsWith( "--" ) ) {
                        LamuApplicationNamedArgument a = new LamuApplicationNamedArgument(s);
                        switch ( a.getKey() ) {
//                                case "port" : 
//                                    portNumberList.add(  Integer.parseInt( a.value ) );
//                                    break;
                            default :
                                throw new RuntimeException( "unknown parameter : " + a.getKey() );
                        }
                    } else {
                        fileNameList.add(s);
                    }
                    return this;
                }
                @Override
                public void notifyEnd(ArgumentParser parser) {
                    if ( parser.getValueStack( SCHEME_ENGINE ).isEmpty() ) {
                        throw new RuntimeException( "no scheme is defined." );
                    }
                    SchemeEngine schemeEngine = parser.getValueStack( SCHEME_ENGINE ).peek();
                    
                    PulsarFrame frame = LamuApplicationLibrary.createPulsarGui( schemeEngine );
                    if (parser.getValueStack( FRAME ).size() == 0 )
                        frame.setShutdownWhenClose( true );
                    else
                        frame.setShutdownWhenClose( false );
                    
                    parser.getValueStack( FRAME ).push( frame );
                    parser.getValueStack( KAWAPAD ).push( frame.getKawapad() );
                    
                    parser.getValueStack( RUNNABLE ).push( new KawapadLoader( frame, fileNameList ));
                }
            };
        }
    }

    static final class KawapadGuiArgumentParserElementFactory implements ArgumentParserElementFactory {
        @Override
        public ArgumentParserElement create() {
            List<String> fileNameList = new ArrayList<>();
            return new ArgumentParserElement() {
                @Override
                public ArgumentParserElement notifyArg(ArgumentParser parser, String s) {
                    if ( s.startsWith( "--" ) ) {
                        LamuApplicationNamedArgument a = new LamuApplicationNamedArgument(s);
                        switch ( a.getKey() ) {
//                                case "port" : 
//                                    portNumberList.add(  Integer.parseInt( a.value ) );
//                                    break;
                            default :
                                throw new RuntimeException( "unknown parameter : " + a.getKey() );
                        }
                    } else {
                        fileNameList.add(s);
                    }
                    return this;
                }
                @Override
                public void notifyEnd(ArgumentParser parser) {
                    if ( parser.getValueStack( SCHEME_ENGINE ).isEmpty() ) {
                        throw new RuntimeException( "no scheme is defined." );
                    }
                    SchemeEngine schemeEngine = parser.getValueStack( SCHEME_ENGINE ).peek();
                    KawapadFrame frame = LamuApplicationLibrary.createKawapad( schemeEngine );
                    
                    if (parser.getValueStack( FRAME ).size() == 0 )
                        frame.setShutdownWhenClose( true );
                    else
                        frame.setShutdownWhenClose( false );
                    
                    parser.getValueStack( FRAME ).push( frame );
                    parser.getValueStack( KAWAPAD ).push( frame.getKawapad() );
                    parser.getValueStack( RUNNABLE ).push( new KawapadLoader( frame, fileNameList ));
                    parser.getValueStack( RUNNABLE ).push( new Runnable() {
                        @Override
                        public void run() {
//                                pulsar.init();
                        }
                    });
                }
            };
        }
    }

    static final class PulsarArgumentParserElementFactory implements ArgumentParserElementFactory {
        @Override
        public ArgumentParserElement create() {
            return new ArgumentParserElement() {
                @Override
                public ArgumentParserElement notifyArg(ArgumentParser parser, String s) {
                    throw new RuntimeException( "unknown parameter : " + s);
                }
                @Override
                public void notifyEnd(ArgumentParser parser) {
                    if ( parser.getValueStack( SCHEME_ENGINE ).isEmpty() ) {
                        throw new RuntimeException( "no scheme is defined." );
                    }
                    SchemeEngine schemeEngine = parser.getValueStack( SCHEME_ENGINE ).peek();
                    Pulsar pulsar = LamuApplicationLibrary.createPulsar( schemeEngine );
                    parser.getValueStack( PULSAR ).push( pulsar );
                    
                    parser.getValueStack( RUNNABLE ).push( new Runnable() {
                        @Override
                        public void run() {
//                                pulsar.init();
                        }
                    });
                }
            };
        }
    }

    static final class SchemeArgumentParserElementFactory implements ArgumentParserElementFactory {
        static final String DEFAULT_REMOTE_URL = "http://localhost:";
        @Override
        public ArgumentParserElement create() {
            return new ArgumentParserElement() {
                SchemeEngine schemeEngine = new SchemeEngine();
                {
                    LamuApplicationLibrary.initializeSchemeEngine( schemeEngine );
                }
                
                List<String> urlList = new ArrayList<>();
                @Override
                public ArgumentParserElement notifyArg(ArgumentParser parser, String s) {
                    if ( s.startsWith( "--"  ) ) {
                        LamuApplicationNamedArgument a = new LamuApplicationNamedArgument( s );
                        if ( false ) {
                            //
                        } else if ( "server-url".equals( a.getKey() ) ) {
                            urlList.add( a.getValue() );
                        } else if ( "server-port".equals( a.getKey() ) ) {
                            urlList.add( DEFAULT_REMOTE_URL + a.getValue() );
                        } else {
                            throw new RuntimeException( "unknown argument " + s );
                        }
                    } else {
                        throw new RuntimeException( "unknown command " + s );
                    }
                    return this;
                }
                @Override
                public void notifyEnd(ArgumentParser parser) {
                    EvaluatorManager.initEvaluatorManager( 
                        schemeEngine.getEvaluatorManager(), 
                        this.urlList );
                    
//                        schemeSecretary.setDirectMeeting( directMeeting );
                    parser.getValueStack( SCHEME_ENGINE ).push( this.schemeEngine );

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

    {
        registerFactory( "scheme", new SchemeArgumentParserElementFactory());
        registerFactory( "pulsar", new PulsarArgumentParserElementFactory());
        registerFactory( "kawapad-gui", new KawapadGuiArgumentParserElementFactory());
        registerFactory( "pulsar-gui", new PulsarGuiArgumentParserElementFactory());
        registerFactory( "scheme-server", new SchemeServerArgumentParserElementFactory());
        registerFactory( "output-reference", new OutputReferenceArgumentParserElementFactory());
        registerFactory( "all-available-references", new AllAvailableReferenceArgumentParserElementFactory());
    }
}
