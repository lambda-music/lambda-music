package lamu.lib.scheme.repl;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.lang.invoke.MethodHandles;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;

import javax.swing.JButton;
import javax.swing.JFrame;

import lamu.lib.log.Logger;
import lamu.lib.stream.SisoReceiver;
import lamu.lib.stream.SisoReceiverListener;
import lamu.lib.stream.SisoReceiverMessage;

public class ReplClient extends ReplClientServer {
    public static interface ReplClientResultReceiver {
        void receive( String result );
    }

    protected static final Logger LOGGER = Logger.getLogger(MethodHandles.lookup().lookupClass().getName());
    protected static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    protected static void logInfo(String msg) { LOGGER.log(Level.INFO, msg); }
    protected static void logWarn(String msg) { LOGGER.log(Level.WARNING, msg); }
    protected static void logWarn(Throwable e) { LOGGER.log(Level.WARNING, "warning", e); }

    protected SisoReceiver receiver;
    public ReplClient( String prefix ) {
        super( prefix );
    }
    public ReplClient() {
        super();
    }

    @Override
    public void start( SisoReceiver receiver ) {
    }
    @Override
    public void end(SisoReceiver receiver) {
    }
    
    @Override
    public void notifyParent(SisoReceiver receiver) {
        this.receiver = receiver;
    }
    
    final Map<String,ReplClientResultReceiver> resultReceiverMap = new HashMap<>();
    
    public void exec( String schemeScript, ReplClientResultReceiver resultReceiver ) {
        if ( resultReceiver == null )
            throw new IllegalArgumentException( "null is not allowed" );
        
        String sessionName = createSession();
        
        StringBuilder sb = new StringBuilder();
        sb.append( createCommandString( "session", sessionName ) );
        sb.append( "\n" );
        sb.append( escapeText( schemeScript ) );
        sb.append( "\n" );
        sb.append( createCommandString( "exec", "" ) );
        
        SisoReceiverMessage message = SisoReceiver.createPrintMessage( sb.toString() );
        
        resultReceiverMap.put( sessionName, resultReceiver );
        
        receiver.postMessage( message );
    }
    public void reset() {
        // TODO
    }
    
    public String createSession() {
        return String.format( "session-%08x", random.nextInt() );
    }
    
    {
        registerCommand( "done", new SisoReceiverListener() {
            @Override
            public void process( SisoReceiver receiver, String argument ) {
                argument = argument.trim();

                String result = getCurrentBuffer().trim();
                clearCurrentBuffer();
                
                String currentSession = fetchCurrentSession();

                if ( currentSession == null ) {
                    logInfo("ignored session: [default-session]");
                    return;
                } else if (  ! resultReceiverMap.containsKey( currentSession ) ) {
                    // if the session name is unknown, just ignore it.
                    // this would happen if the stream is shared with other processes.
                    logInfo("ignored session:" + currentSession);
                    return;
                }

                ReplClientResultReceiver resultReceiver = resultReceiverMap.remove( currentSession );
                
                resultReceiver.receive( result );
            }
        });
    }
    

    @Override
    protected void onReplTerminate( SisoReceiver receiver, String s ) {
        // Execute the current buffer when the stream is terminated.
        callCommand( "exec" , receiver, "" );
//        callCommand( "quit" , receiver, "" );
    }


    
    public static void main(String[] args) {
        ReplClient c = new ReplClient( ";" );
        new SisoReceiver( null, System.in, System.out, c ).requestInit();
        {
            JFrame frame = new JFrame();
            frame.setDefaultCloseOperation( JFrame.EXIT_ON_CLOSE );
            JButton b = new JButton();
            b.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    c.exec("hello", new ReplClientResultReceiver( ) {
                        @Override
                        public void receive(String result) {
                            System.err.println( result );
                        }
                    });
                }
            });
            frame.getContentPane().add( b );
            frame.setSize(300, 300);
            frame.setVisible(true);
        }
       
    }
}
