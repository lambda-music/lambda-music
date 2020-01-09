package pulsar;

import java.io.File;
import java.io.IOException;
import java.lang.invoke.MethodHandles;
import java.lang.management.ManagementFactory;
import java.lang.management.RuntimeMXBean;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.logging.Level;

import kawapad.Kawapad;
import kawapad.KawapadFrame;
import pulsar.lib.app.ApplicationComponent;
import pulsar.lib.app.ApplicationVessel;
import pulsar.lib.log.PulsarLogger;
import pulsar.lib.scheme.EvaluatorManager;
import pulsar.lib.scheme.SchemeEngine;
import pulsar.lib.scheme.doc.DescriptiveDocumentCategory;
import pulsar.lib.scheme.http.SchemeHttp;
import pulsar.lib.scheme.http.SchemeHttp.UserAuthentication;
import pulsar.lib.thread.ThreadInitializer;
import pulsar.lib.thread.ThreadInitializerCollection;
import pulsar.lib.thread.ThreadInitializerCollectionContainer;
import pulsar.lib.thread.ThreadInitializerContainer;

class PulsarApplicationArgumentParser {
    static final PulsarLogger LOGGER = PulsarLogger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }

    static final String DEFAULT_REMOTE_URL = "http://localhost:";
    ArrayList<ApplicationVessel> applicationVesselList = new ArrayList<>();
    ArrayDeque<SchemeEngine> schemeEngineStack = new ArrayDeque<>();
    ArrayDeque<Pulsar> pulsarStack= new ArrayDeque<>();
    ArrayDeque<Kawapad> kawapadStack= new ArrayDeque<>();
    ArrayDeque<KawapadFrame> frameStack= new ArrayDeque<>();
    ArrayDeque<SchemeHttp> schemeHttpStack= new ArrayDeque<>();
    ArrayDeque<Runnable> runnableStack = new ArrayDeque<>();
    
    public List<ApplicationVessel> getApplicationVesselList() {
        return Collections.unmodifiableList( applicationVesselList );
    }

    ArrayList<ArrayDeque> allDeque = new ArrayList<>();
    {
        allDeque.add(schemeEngineStack);
        allDeque.add(pulsarStack);
        allDeque.add(kawapadStack);
        allDeque.add(frameStack);
        allDeque.add(schemeHttpStack);
//        allDeque.add(runnableStack); XXX
    }
    
    {
        RuntimeMXBean runtimeMxBean = ManagementFactory.getRuntimeMXBean();
        List<String> arguments = runtimeMxBean.getInputArguments();
        
        System.out.println( arguments );
        System.out.println( "getBootClassPath:"+runtimeMxBean.getBootClassPath() );
        System.out.println( "getClassPath:"+runtimeMxBean.getClassPath() );
    }
    
    private static void addInitializerContainer( Collection<ThreadInitializer> destination, Collection source ) {
        for ( Object o : source ) {
            if ( o instanceof ThreadInitializerContainer ) {
                destination.add(((ThreadInitializerContainer)o).getThreadInitializer());
                // Add the only first one.  (Fri, 20 Dec 2019 05:01:24 +0900)
                break;
            }
        }
    }
    private static void addInitializerCollectionContainer( Collection<ThreadInitializerCollection> destination, Collection source ) {
        for ( Object o : source ) {
            if ( o instanceof ThreadInitializerCollectionContainer ) {
                destination.add(((ThreadInitializerCollectionContainer)o).getThreadInitializerCollection());
            }
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

    static class RunnableInitializer implements ApplicationComponent {
        private ApplicationComponent parent;
        @Override
        public void setParentApplicationComponent(ApplicationComponent parent) {
            this.parent = parent;
        }

        @Override
        public ApplicationComponent getParentApplicationComponent() {
            return parent;
        }
        
        final List<Runnable> runnableStack = new ArrayList<>();
        public RunnableInitializer( List<Runnable> runnableStack ) {
            this.runnableStack.addAll( runnableStack );
        }

        @Override
        public void processInit() {
            for ( Runnable r : runnableStack ) {
                try {
                    System.err.println( "invoke:"+ r  );
                    r.run();
                } catch (Exception e) {
                    logError( "", e );
                }
            }
        }
        @Override
        public void processQuit() {
        }
    }
    
    
    void deploy() {
       
        // Collect thread initializers and set to collections.
        ArrayList<ThreadInitializer> threadInitializerList = new ArrayList<>(); 
        ArrayList<ThreadInitializerCollection> threadInitializerCollectionList = new ArrayList<>(); 
        {
            // Collect all thread initializers.
            for ( ArrayDeque deque : allDeque ) {
                addInitializerContainer( threadInitializerList, deque );
            }
            // Collect all thread initializer collections.
            for ( ArrayDeque deque : allDeque ) {
                addInitializerCollectionContainer( threadInitializerCollectionList, deque );
            }
            // then, add the initializers to the collections.
            for ( ThreadInitializerCollection c : threadInitializerCollectionList ) {
                c.addAllThreadInitializer( threadInitializerList );
            }
        }
        
        ApplicationVessel vessel = new ApplicationVessel();
        vessel.getThreadInitializerCollection().addAllThreadInitializer( threadInitializerList );
        vessel.addAll( schemeEngineStack );
        vessel.addAll( pulsarStack );
        vessel.addAll( kawapadStack );
        vessel.addAll( frameStack );
        vessel.addAll( schemeHttpStack );
        
        // Executing runnable stack;
        {
            ArrayList<Runnable> list = new ArrayList<Runnable>( this.runnableStack );
            Collections.reverse( list );
            vessel.add( new RunnableInitializer( list ) );
        }
 
        applicationVesselList.add( vessel );

//        vessel.getThreadInitializerCollection().initialize();
//        
//        for ( ThreadInitializerCollection c : threadInitializerCollectionList ) {
//            logInfo( "deploy : "+ c.id + ":" + c.toString() );
//        }
//        vessel.processInit();

        //

        runnableStack.clear();
        schemeEngineStack.clear();
        pulsarStack.clear();
        frameStack.clear();
        schemeHttpStack.clear();
    }
    abstract class Element {
        /*
         * The returned value becomes to the "current" element afterwards;
         * usually it should return "this".
         */
        abstract Element notifyArg( String s );
        
        abstract void notifyEnd();
    }
    abstract class ElementFactory {
        abstract Element create();
    }
    HashMap<String,ElementFactory> factoryMap = new HashMap<>();
    {
        factoryMap.put( "default", new ElementFactory() {
            @Override
            Element create() {
                return new Element() {
                    @Override
                    Element notifyArg(String s) {
                        ElementFactory f = factoryMap.get( s );
                        if ( f == null ) {
                            throw new RuntimeException( "unknown command \"" + s + "\"" );
                        } else {
                            return f.create(); 
                        }
                    }
                    @Override
                    void notifyEnd() {
                    }
                };
            }
        });
        factoryMap.put( "scheme", new ElementFactory() {
            @Override
            Element create() {
                return new Element() {
                    SchemeEngine schemeEngine = new SchemeEngine();
                    {
                        PulsarApplicationLibrary.initializeSchemeEngine( schemeEngine );
                    }
                    
                    List<String> urlList = new ArrayList<>();
                    @Override
                    Element notifyArg(String s) {
                        if ( s.startsWith( "--"  ) ) {
                            PulsarApplicationNamedArgument a = new PulsarApplicationNamedArgument( s );
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
                    void notifyEnd() {
                        EvaluatorManager.initEvaluatorManager( 
                            schemeEngine.getEvaluatorManager(), 
                            this.urlList );
                        
//                        schemeSecretary.setDirectMeeting( directMeeting );
                        schemeEngineStack.push( this.schemeEngine );

//                        runnableStack.push( new Runnable() {
//                            @Override
//                            public void run() {
//                                schemeSecretary.newScheme();
//                            }
//                        });
                    }
                };
            }
        });
        factoryMap.put( "pulsar", new ElementFactory() {
            @Override
            Element create() {
                return new Element() {
                    @Override
                    Element notifyArg(String s) {
                        throw new RuntimeException( "unknown parameter : " + s);
                    }
                    @Override
                    void notifyEnd() {
                        if ( schemeEngineStack.isEmpty() ) {
                            throw new RuntimeException( "no scheme is defined." );
                        }
                        SchemeEngine schemeEngine = schemeEngineStack.peek();
                        Pulsar pulsar = PulsarApplicationLibrary.createPulsar( schemeEngine );
                        pulsarStack.push( pulsar );
                        
                        runnableStack.push( new Runnable() {
                            @Override
                            public void run() {
//                                pulsar.init();
                            }
                        });
                    }
                };
            }
        });

        factoryMap.put( "kawapad-gui", new ElementFactory() {
            @Override
            Element create() {
                List<String> fileNameList = new ArrayList<>();
                return new Element() {
                    @Override
                    Element notifyArg(String s) {
                        if ( s.startsWith( "--" ) ) {
                            PulsarApplicationNamedArgument a = new PulsarApplicationNamedArgument(s);
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
                    void notifyEnd() {
                        if ( schemeEngineStack.isEmpty() ) {
                            throw new RuntimeException( "no scheme is defined." );
                        }
                        SchemeEngine schemeEngine = schemeEngineStack.peek();
                        KawapadFrame frame = PulsarApplicationLibrary.createKawapad( schemeEngine );
                        
                        if (frameStack.size() == 0 )
                            frame.setShutdownWhenClose( true );
                        else
                            frame.setShutdownWhenClose( false );
                        
                        frameStack.push( frame );
                        kawapadStack.push( frame.getKawapad() );
                        runnableStack.push( new KawapadLoader( frame, fileNameList ));
                        runnableStack.push( new Runnable() {
                            @Override
                            public void run() {
//                                pulsar.init();
                            }
                        });
                    }
                };
            }
        });
        
        factoryMap.put( "pulsar-gui", new ElementFactory() {
            @Override
            Element create() {
                return new Element() {
                    List<String> fileNameList = new ArrayList<>();
                    @Override
                    Element notifyArg(String s) {
                        if ( s.startsWith( "--" ) ) {
                            PulsarApplicationNamedArgument a = new PulsarApplicationNamedArgument(s);
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
                    void notifyEnd() {
                        if ( schemeEngineStack.isEmpty() ) {
                            throw new RuntimeException( "no scheme is defined." );
                        }
                        SchemeEngine schemeEngine = schemeEngineStack.peek();
                        
                        PulsarFrame frame = PulsarApplicationLibrary.createPulsarGui( schemeEngine );
                        if (frameStack.size() == 0 )
                            frame.setShutdownWhenClose( true );
                        else
                            frame.setShutdownWhenClose( false );
                        
                        frameStack.push( frame );
                        kawapadStack.push( frame.getKawapad() );
                        
                        runnableStack.push( new KawapadLoader( frame, fileNameList ));
                    }
                };
            }
        });
        
        
        factoryMap.put( "scheme-server", new ElementFactory() {
            @Override
            Element create() {
                return new Element() {
                    private int serverPort = 8192;
                    // TODO 0 variable accept path for HTTP 
                    private String serverPath = "";
                    UserAuthentication userAuthentication = UserAuthentication.ONLY_LOOPBACK;
                    @Override
                    Element notifyArg(String s) {
                        if (s.startsWith( "--" ) ) {
                            PulsarApplicationNamedArgument a = new PulsarApplicationNamedArgument(s);
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
                    void notifyEnd() {
                        if ( schemeEngineStack.isEmpty() ) {
                            throw new RuntimeException( "no scheme is defined." );
                        }
                        SchemeEngine schemeEngine = schemeEngineStack.peek();
//                        if ( pulsarStack.isEmpty() ) {
//                            throw new RuntimeException( "no pulsar is defined." );
//                        }
//                        Pulsar pulsar = pulsarStack.peek();
                        
                        try {
                            SchemeHttp schemeHttp = PulsarApplicationLibrary.createPulsarHttpServer( 
                                schemeEngine, serverPort, serverPath, userAuthentication );
                            schemeHttpStack.push( schemeHttp );
                        } catch (IOException e) {
                            throw new RuntimeException( e );
                        }
                    }
                };
            }
        });
        factoryMap.put( "output-reference", new ElementFactory() {
            @Override
            Element create() {
                return new Element() {
                    String outputFile = null;
                    String category   = null;
                    @Override
                    Element notifyArg(String s) {
                        if ( s.startsWith( "--" ) ) {
                            PulsarApplicationNamedArgument narg = new PulsarApplicationNamedArgument( s );
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
                    void notifyEnd() {
                        if ( schemeEngineStack.isEmpty() ) {
                            throw new RuntimeException( "no scheme is defined." );
                        }
                        SchemeEngine schemeEngine = schemeEngineStack.peek();
                        
                        runnableStack.add( new Runnable() {
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
        });
        factoryMap.put( "all-available-references", new ElementFactory() {
            @Override
            Element create() {
                return new Element() {
                    String outputFile = null;
                    @Override
                    Element notifyArg(String s) {
                        if ( s.startsWith( "--" ) ) {
                            PulsarApplicationNamedArgument narg = new PulsarApplicationNamedArgument( s );
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
                    void notifyEnd() {
                        if ( schemeEngineStack.isEmpty() ) {
                            throw new RuntimeException( "no scheme is defined." );
                        }
                        runnableStack.add( new Runnable() {
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
        });
    }
    
    Element defaultElement = factoryMap.get( "default" ).create();
    Element currentElement = defaultElement;
    private void notifyEnd() {
        if ( currentElement != null ) {
            // If currentElement is default element, notifyEnd() does not do anything;
            // otherwise calling notifyEnd() does its clean up process.
            // (Thu, 05 Dec 2019 15:32:18 +0900)
            currentElement.notifyEnd();
        }
    }
    private void notifyArg(String s) {
        Element nextElement;
        if ( "+".equals( s ) ) {
            nextElement = defaultElement;
        } else {
            nextElement = currentElement.notifyArg( s );
        }
        if ( nextElement != currentElement ) {
            notifyEnd();
        }
        currentElement=nextElement;
    }
    
    public void parse( String[] args ) {
        for ( int i=0; i<args.length; i++ ) {
            String s = args[i];
            PulsarApplication.logInfo( "args[" + i +"]=\"" + s  + "\"");
            notifyArg( s );
        }
        notifyEnd();
        notifyAllEnd();
    }
    private void notifyAllEnd() {
        deploy();
    }
}
