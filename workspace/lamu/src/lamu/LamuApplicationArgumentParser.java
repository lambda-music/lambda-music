package lamu;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.lang.invoke.MethodHandles;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import kawapad.Kawapad;
import kawapad.KawapadFrame;
import pulsar.Pulsar;
import pulsar.PulsarFrame;
import quartz.lib.app.args.ArgumentParser;
import quartz.lib.app.args.ArgumentParserDefault;
import quartz.lib.app.args.ArgumentParserElement;
import quartz.lib.app.args.ArgumentParserElementFactory;
import quartz.lib.app.args.ArgumentParserStackKey;
import quartz.lib.log.LamuLogger;
import quartz.lib.scheme.EvaluatorManager;
import quartz.lib.scheme.SchemeEngine;
import quartz.lib.scheme.doc.DescriptiveDocumentCategory;
import quartz.lib.scheme.socket.ReplSisoListener;
import quartz.lib.scheme.socket.SchemeHttp;
import quartz.lib.scheme.socket.SchemeHttp.UserAuthentication;
import quartz.lib.scheme.socket.SisoReceiver;

class LamuApplicationArgumentParser extends ArgumentParserDefault {
    static final Logger LOGGER = LamuLogger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }

    static final String MSG_NO_SCHEME_ERROR     = "no scheme engine was instanciated";
    static final String MSG_OUTPUT_HELP_ERROR   = "an error occured in output-help";
    static final String MSG_UNKNOWN_PARAM_ERROR = "unknown parameter : ";
    
    @Override
    protected void createValueStackMap(){
        getValueStack( SCHEME_ENGINE );
        getValueStack( PULSAR );
        getValueStack( KAWAPAD );
        getValueStack( REPL );
        getValueStack( FRAME );
        getValueStack( SCHEME_HTTP );
        getValueStack( RUNNABLE );
    }

    static final ArgumentParserStackKey<SchemeEngine> SCHEME_ENGINE = new ArgumentParserStackKey<>();
    static final ArgumentParserStackKey<Pulsar> PULSAR = new ArgumentParserStackKey<>();
    static final ArgumentParserStackKey<Kawapad> KAWAPAD = new ArgumentParserStackKey<>();
    static final ArgumentParserStackKey<SisoReceiver> REPL = new ArgumentParserStackKey<>();
    static final ArgumentParserStackKey<KawapadFrame> FRAME = new ArgumentParserStackKey<>();
    static final ArgumentParserStackKey<SchemeHttp> SCHEME_HTTP= new ArgumentParserStackKey<>();
    static final ArgumentParserStackKey<SisoReceiver> STPD_RECEIVER = new ArgumentParserStackKey<>();
    
    static final class AllAvailableReferenceArgumentParserElementFactory implements ArgumentParserElementFactory {
        @Override
        public ArgumentParserElement create() {
            return new ArgumentParserElement() {
                String outputFile = null;
                @Override
                public ArgumentParserElement notifyArg(ArgumentParser parser, String s) {
                    if ( s.startsWith( "--" ) ) {
                        LamuNamedArgument narg = new LamuNamedArgument( s );
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
                public void notifyEnd( ArgumentParser parser ) {
                    if ( parser.getValueStack( SCHEME_ENGINE ).isEmpty() ) {
                        throw new RuntimeException( MSG_NO_SCHEME_ERROR );
                    }
                    LamuApplication.loadBasicClasses();
                    parser.getValueStack( RUNNABLE ).add( new Runnable() {
                        @Override
                        public void run() {
                            try {
                                DescriptiveDocumentCategory.outputAvailableReferences( outputFile );
                            } catch (IOException e) {
                                logError( MSG_OUTPUT_HELP_ERROR, e ); 
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
                        LamuNamedArgument narg = new LamuNamedArgument( s );
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
                public void notifyEnd(ArgumentParser parser) {
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
                void procKeyStroke(ArgumentParser parser) {
					if ( parser.getValueStack( SCHEME_ENGINE ).isEmpty() ) {
						throw new RuntimeException( MSG_NO_SCHEME_ERROR );
					}
					SchemeEngine schemeEngine = parser.getValueStack( SCHEME_ENGINE ).peek();

					LamuApplication.loadBasicClasses();
					parser.getValueStack( RUNNABLE ).add( new Runnable() {
						@Override
						public void run() {
							Kawapad kawapad = new Kawapad(schemeEngine);
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
				void procDocument(ArgumentParser parser) {
					if ( parser.getValueStack( SCHEME_ENGINE ).isEmpty() ) {
						throw new RuntimeException( MSG_NO_SCHEME_ERROR );
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
								logError( MSG_OUTPUT_HELP_ERROR, e ); 
							}
						}
					});
				}
            };
        }
    }

    static final class SchemeTransferProtocolArgumentParserElementFactory implements ArgumentParserElementFactory {
        @Override
        public ArgumentParserElement create() {
            return new ArgumentParserElement() {
//                int port = 0;
                @Override
                public ArgumentParserElement notifyArg( ArgumentParser parser, String s ) {
                	if ( false ) {
                		
//                	} else if ( s.startsWith( "--" ) ) {
//                        LamuNamedArgument a = new LamuNamedArgument(s);
//                        switch ( a.getKey() ) {
//                        case "port" : 
//                        	port = Integer.parseInt( a.getValue() );
//                        	break;
//                            default :
//                                throw new RuntimeException( MSG_UNKNOWN_PARAM_ERROR + a.getKey() );
//                        }
                    } else {
                        throw new RuntimeException( MSG_UNKNOWN_PARAM_ERROR + s );
                    }
                    return this;
                }
                @Override
                public void notifyEnd(ArgumentParser parser) {
                    if ( parser.getValueStack( SCHEME_ENGINE ).isEmpty() ) {
                        throw new RuntimeException( MSG_NO_SCHEME_ERROR );
                    }
                    SchemeEngine schemeEngine = parser.getValueStack( SCHEME_ENGINE ).peek();
                    SisoReceiver sisoReceiver = new SisoReceiver( null, System.in, System.out, new ReplSisoListener( schemeEngine ) );
                    parser.getValueStack( STPD_RECEIVER ).push( sisoReceiver );
                }
            };
        }
    }

    static final class SchemeHttpServerArgumentParserElementFactory implements ArgumentParserElementFactory {
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
                        LamuNamedArgument a = new LamuNamedArgument(s);
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
                public void notifyEnd(ArgumentParser parser) {
                    if ( parser.getValueStack( SCHEME_ENGINE ).isEmpty() ) {
                        throw new RuntimeException( MSG_NO_SCHEME_ERROR );
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
                        LamuNamedArgument a = new LamuNamedArgument(s);
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
                public void notifyEnd(ArgumentParser parser) {
                    if ( parser.getValueStack( SCHEME_ENGINE ).isEmpty() ) {
                        throw new RuntimeException( MSG_NO_SCHEME_ERROR );
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
                        LamuNamedArgument a = new LamuNamedArgument(s);
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
                public void notifyEnd(ArgumentParser parser) {
                    if ( parser.getValueStack( SCHEME_ENGINE ).isEmpty() ) {
                        throw new RuntimeException( MSG_NO_SCHEME_ERROR );
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
                    throw new RuntimeException( MSG_UNKNOWN_PARAM_ERROR + s);
                }
                @Override
                public void notifyEnd(ArgumentParser parser) {
                    if ( parser.getValueStack( SCHEME_ENGINE ).isEmpty() ) {
                        throw new RuntimeException( MSG_NO_SCHEME_ERROR );
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
    static final class ReplArgumentParserElementFactory implements ArgumentParserElementFactory {
        @Override
        public ArgumentParserElement create() {
            return new ArgumentParserElement() {
                @Override
                public ArgumentParserElement notifyArg(ArgumentParser parser, String s) {
                    throw new RuntimeException( MSG_UNKNOWN_PARAM_ERROR + s);
                }
                @Override
                public void notifyEnd(ArgumentParser parser) {
                    if ( parser.getValueStack( SCHEME_ENGINE ).isEmpty() ) {
                        throw new RuntimeException( MSG_NO_SCHEME_ERROR );
                    }
                    SchemeEngine schemeEngine = parser.getValueStack( SCHEME_ENGINE ).peek();
                    
                    parser.getValueStack( REPL ).push( new SisoReceiver( null, System.in, System.out, new ReplSisoListener( schemeEngine ) ) );
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
                        LamuNamedArgument a = new LamuNamedArgument( s );
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

    /**
	 * Sets up the commands for the command-line parser. Please read the code of the 
	 * {@link LamuApplicationArgumentParser#initializeParser()} 
	 * <pre>{@code
	 * registerFactory("scheme",           new SchemeArgumentParserElementFactory());
	 * registerFactory("kawapad",          new KawapadGuiArgumentParserElementFactory());
	 * registerFactory("pulsar",           new PulsarArgumentParserElementFactory());
	 * registerFactory("gui",              new PulsarGuiArgumentParserElementFactory());
	 * registerFactory("httpd",            new SchemeServerArgumentParserElementFactory());
	 * registerFactory("output-help",      new OutputReferenceArgumentParserElementFactory());
	 * registerFactory("output-help-list", new AllAvailableReferenceArgumentParserElementFactory());
	 * }</pre>
	 */
	protected void initializeParser() {
		registerFactory( "scheme",           new SchemeArgumentParserElementFactory());
        registerFactory( "kawapad",          new KawapadGuiArgumentParserElementFactory());
        registerFactory( "pulsar",           new PulsarArgumentParserElementFactory());
        registerFactory( "repl",             new ReplArgumentParserElementFactory());
        registerFactory( "gui",              new PulsarGuiArgumentParserElementFactory());
        registerFactory( "httpd",            new SchemeHttpServerArgumentParserElementFactory());
        registerFactory( "stpd",             new SchemeTransferProtocolArgumentParserElementFactory());
        registerFactory( "output-help",      new OutputReferenceArgumentParserElementFactory());
        registerFactory( "output-help-list", new AllAvailableReferenceArgumentParserElementFactory());
	}
    {
        initializeParser();
    }
}
