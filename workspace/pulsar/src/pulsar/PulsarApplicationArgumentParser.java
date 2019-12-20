package pulsar;

import java.io.File;
import java.io.IOException;
import java.lang.invoke.MethodHandles;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import kawapad.Kawapad;
import kawapad.KawapadFrame;
import pulsar.lib.app.ApplicationVessel;
import pulsar.lib.scheme.http.SchemeHttp;
import pulsar.lib.scheme.http.SchemeHttp.UserAuthentication;
import pulsar.lib.scheme.scretary.SchemeSecretary;
import pulsar.lib.thread.ThreadInitializer;
import pulsar.lib.thread.ThreadInitializerCollection;
import pulsar.lib.thread.ThreadInitializerCollectionContainer;
import pulsar.lib.thread.ThreadInitializerContainer;

class PulsarApplicationArgumentParser {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }

    
    ArrayList<ApplicationVessel> applicationVesselList = new ArrayList<>();
    ArrayDeque<SchemeSecretary> schemeSecretaryStack = new ArrayDeque<>();
    ArrayDeque<Pulsar> pulsarStack= new ArrayDeque<>();
    ArrayDeque<PulsarFrame> pulsarFrameStack= new ArrayDeque<>();
    ArrayDeque<Kawapad> kawapadStack= new ArrayDeque<>();
    ArrayDeque<KawapadFrame> kawapadFrameStack= new ArrayDeque<>();
    ArrayDeque<SchemeHttp> schemeHttpStack= new ArrayDeque<>();
    ArrayDeque<Runnable> runnableStack = new ArrayDeque<>();
    
    ArrayList<ArrayDeque> allDeque = new ArrayList<>();
    {
        allDeque.add(schemeSecretaryStack);
        allDeque.add(pulsarStack);
        allDeque.add(pulsarFrameStack);
        allDeque.add(kawapadStack);
        allDeque.add(kawapadFrameStack);
        allDeque.add(schemeHttpStack);
        allDeque.add(runnableStack);
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
    
    void deploy() {
        if ( false ) {
            for ( SchemeSecretary i : schemeSecretaryStack ) {
                i.newScheme();
            }
            for ( Pulsar i : pulsarStack ) {
                try {
                    i.init();
                } catch (Exception e) {
                    logError( "", e );
                }
            }
            for ( PulsarFrame i : pulsarFrameStack ) {
                try {
                    i.init();
                } catch (Exception e) {
                    logError( "", e );
                }
            }
            for ( Kawapad i : kawapadStack ) {
                try {
                    i.init();
                } catch (Exception e) {
                    logError( "", e );
                }
            }
            for ( KawapadFrame i : kawapadFrameStack ) {
                try {
                    i.init();
                } catch (Exception e) {
                    logError( "", e );
                }
            }
            for ( SchemeHttp i : schemeHttpStack ) {
                try {
                    i.init();
                } catch (Exception e) {
                    logError( "", e );
                }
            }
        }
        
        {
            ArrayList<Runnable> list = new ArrayList<Runnable>( this.runnableStack );
            Collections.reverse( list );
            for ( Runnable r : list ) {
                try {
                    System.err.println( "invoke:"+ r  );
                    r.run();
                } catch (Exception e) {
                    logError( "", e );
                }
            }
        }
        
//        for ( Runnable i : runnableStack ) {
//            try {
//                logInfo( "invoke:"+ i );
//                i.run();
//            } catch (Exception e) {
//                logError( "", e );
//            }
//        }
    
        // Collect all thread initializers.
        ArrayList<ThreadInitializer> threadInitializerList = new ArrayList<>(); 
        for ( ArrayDeque deque : allDeque ) {
            addInitializerContainer( threadInitializerList, deque );
        }
        // Collect all thread initializer collections.
        ArrayList<ThreadInitializerCollection> threadInitializerCollectionList = new ArrayList<>(); 
        for ( ArrayDeque deque : allDeque ) {
            addInitializerCollectionContainer( threadInitializerCollectionList, deque );
        }
        // then, add the initializers to the collections.
        for ( ThreadInitializerCollection c : threadInitializerCollectionList ) {
            c.addAllThreadInitializer( threadInitializerList );
        }
        
        ApplicationVessel vessel = new ApplicationVessel();
        vessel.getThreadInitializerCollection().addAllThreadInitializer( threadInitializerList );
        
        vessel.addAll( schemeSecretaryStack );
        vessel.addAll( pulsarStack );
        vessel.addAll( pulsarFrameStack );
        vessel.addAll( kawapadStack );
        vessel.addAll( kawapadFrameStack );
        vessel.addAll( schemeHttpStack );
 
        applicationVesselList.add( vessel );

        vessel.requesetInit();
        
        for ( ThreadInitializerCollection c : threadInitializerCollectionList ) {
            logInfo( "deploy : "+ c.id + ":" + c.toString() );
        }
        
        //

        runnableStack.clear();
        schemeSecretaryStack.clear();
        pulsarStack.clear();
        pulsarFrameStack.clear();
        kawapadFrameStack.clear();
        schemeHttpStack.clear();
        
        
    }
    static final Pattern parseArgPattern = Pattern.compile( "^--([a-zA-Z0-9\\_\\-]+)\\=(.*)$" );
    class NamedArgument {
        String key;
        String value;
        public NamedArgument( String s ) {
            Matcher m = parseArgPattern.matcher( s );
            if ( m.matches() ) {
                this.key = m.group( 1 );
                this.value = m.group( 2 );
            } else {
                this.key = s;
                this.value = null;
            }
        }
    }
    abstract class Element {
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
                    SchemeSecretary schemeSecretary = new SchemeSecretary();
                    {
                        PulsarApplicationLibrary.initializeSchemeSecretary( schemeSecretary );
                    }
                    
                    boolean directMeeting = true;
                    @Override
                    Element notifyArg(String s) {
                        if ( s.startsWith( "--"  ) ) {
                            NamedArgument a = new NamedArgument( s );
                            if ( "parallel".equals( a.key ) ) {
                                this.directMeeting =  true ;
                            } else if ( "serial".equals( a.key) ) {
                                this.directMeeting =  false ;
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
                        schemeSecretaryStack.push( this.schemeSecretary );
                        runnableStack.push( new Runnable() {
                            @Override
                            public void run() {
                                schemeSecretary.setDirectMeeting( directMeeting );
                                schemeSecretary.newScheme();
                            }
                        });
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
                        if ( schemeSecretaryStack.isEmpty() ) {
                            throw new RuntimeException( "no scheme is defined." );
                        }
                        SchemeSecretary schemeSecretary = schemeSecretaryStack.peek();
                        Pulsar pulsar = PulsarApplicationLibrary.createPulsar( schemeSecretary );
                        pulsarStack.push( pulsar );
                        
                        runnableStack.push( new Runnable() {
                            @Override
                            public void run() {
                                pulsar.init();
                            }
                        });
                    }
                };
            }
        });
        
        factoryMap.put( "gui", new ElementFactory() {
            @Override
            Element create() {
                return new Element() {
                    List<Integer> portNumberList = new ArrayList<>();
                    List<String> fileNameList = new ArrayList<>();
                    @Override
                    Element notifyArg(String s) {
                        if ( s.startsWith( "--" ) ) {
                            NamedArgument a = new NamedArgument(s);
                            switch ( a.key ) {
                                case "port" : 
                                    portNumberList.add(  Integer.parseInt( a.value ) );
                                    break;
                                default :
                                    throw new RuntimeException( "unknown parameter : " + a.key );
                            }
                        } else {
                            fileNameList.add(s);
                        }
                        return this;
                    }
                    @Override
                    void notifyEnd() {
                        if ( schemeSecretaryStack.isEmpty() ) {
                            throw new RuntimeException( "no scheme is defined." );
                        }
                        SchemeSecretary schemeSecretary = schemeSecretaryStack.peek();
                        if ( pulsarStack.isEmpty() ) {
                            throw new RuntimeException( "no pulsar is defined." );
                        }
                        Pulsar pulsar = pulsarStack.peek();
                        
                        List<String> urlList = new ArrayList<String>();
                        for ( Integer httpPort : portNumberList ) {
                            urlList.add( "http://localhost:"+httpPort+"/eval" );
                        }
                        
                        PulsarFrame pulsarFrame = PulsarApplicationLibrary.createPulsarGui( schemeSecretary, pulsar, urlList );
                        
                        pulsarFrameStack.push( pulsarFrame );
                        kawapadStack.push( pulsarFrame.getKawapad() );
                        
                        runnableStack.push( new Runnable() {
                            @Override
                            public void run() {
                                pulsarFrame.init();
                                boolean first=true;
                                for ( String s : fileNameList ) {
                                    try {
                                        if ( first ) { 
                                            pulsarFrame.openFile( new File(s) );
                                        } else {
                                            pulsarFrame.getKawapad().createKawapadFrame( new File(s) );
                                        }
                                    } catch (IOException e) {
                                        e.printStackTrace();
                                    }
                                    first=false;
                                }
                                if ( first ) {
                                    try {
                                        pulsarFrame.openIntro();
                                    } catch (IOException e) {
                                        e.printStackTrace();
                                    }
                                }
                            }
                        });
                    }
                };
            }
        });
        
        
        factoryMap.put( "http", new ElementFactory() {
            @Override
            Element create() {
                return new Element() {
                    private int httpPort = 8192;
                    UserAuthentication userAuthentication = UserAuthentication.ONLY_LOOPBACK;
                    @Override
                    Element notifyArg(String s) {
                        if (s.startsWith( "--" ) ) {
                            NamedArgument a = new NamedArgument(s);
                            switch ( a.key ) {
                                case "port" : 
                                    this.httpPort = Integer.parseInt( a.value );
                                    break;
                                case "auth" : 
                                    if ( "only-loopback".equals( a.value )  ) {
                                        this.userAuthentication = UserAuthentication.ONLY_LOOPBACK;
                                        // } else if ( "only-loopback".equals( a.value )  ) {
                                        // TODO
                                    } else {
                                        throw new RuntimeException( "unknown authentication type : " + a.key );
                                    }
                                    break;
                                default :
                                    throw new RuntimeException( "unknown parameter : " + a.key );
                            }
                        } else {
                            throw new RuntimeException( "unknown parameter : " + s );
                        }
                        return this;
                    }
                    @Override
                    void notifyEnd() {
                        if ( schemeSecretaryStack.isEmpty() ) {
                            throw new RuntimeException( "no scheme is defined." );
                        }
                        SchemeSecretary schemeSecretary = schemeSecretaryStack.peek();
                        if ( pulsarStack.isEmpty() ) {
                            throw new RuntimeException( "no pulsar is defined." );
                        }
                        Pulsar pulsar = pulsarStack.peek();
                        
                        try {
                            SchemeHttp schemeHttp = PulsarApplicationLibrary.createPulsarHttpServer( 
                                schemeSecretary, httpPort, userAuthentication, pulsar );
                            schemeHttpStack.push( schemeHttp );
                        } catch (IOException e) {
                            throw new RuntimeException( e );
                        }
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
