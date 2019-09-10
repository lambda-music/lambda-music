package pulsar;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import kawapad.Kawapad;
import kawapad.KawapadDocuments;
import pulsar.lib.PulsarLogger;
import pulsar.lib.scheme.DescriptiveDocumentCategory;
import pulsar.lib.scheme.DescriptiveHelp;
import pulsar.lib.scheme.SchemeUtils;
import pulsar.lib.scheme.http.SchemeHttp;
import pulsar.lib.scheme.scretary.SchemeSecretary;

public class PulsarApplication {

    public PulsarApplication() {
    }
    /**
     * The main method which starts up the application. The application opens a file
     * which is specified in argument values. When more than one arguments were
     * passed to the method, only the first argument is taken. 
     * @throws IOException 
     */
    static Pulsar parseArgsAndStartPulsar( String[] args ) throws IOException {
        boolean argSpecialOutputReference = false;
        String  argSpecialOutputReferenceName = null;
        boolean argSpecialAllAvailableReferences = false;
        boolean argHttp = true;
        int     argHttpPort = 8192;
        boolean argGui = true;
        String  argFileName = null;
        for ( int i=0; i<args.length; i++ ) {
            String s = args[i];

            if ( s.startsWith( "--http=" ) ) {
                argHttp = true;
                argHttpPort = Integer.parseInt( s.substring( "--http=".length() ) );
                break;
            } else if ( s.equals( "--http" ) ) { 
                argHttp = true;
                argHttpPort = 8192;
                break;
            } else if ( s.equals( "--gui" ) ) { 
                argGui = true;
                break;
            } else if ( s.equals( "--no-http" ) ) { 
                argHttp = false;
                break;
            } else if ( s.equals( "--no-gui" ) ) { 
                argGui = false;
                break;
            } else if ( s.equals( "--all-available-references" ) ) { 
                argSpecialAllAvailableReferences = true;
                break;
            } else if ( s.equals( "--output-reference" ) ) { 
                argSpecialOutputReference = true;
                i++;
                if ( i < args.length  ){
                    argSpecialOutputReferenceName = args[i];
                    System.err.println( "--output-reference:" + argSpecialOutputReferenceName  );
                } else {
                    System.err.println( "warning : insufficient nubmer of arguments."
                            + " The given argument --output-reference was ignroed." );  
                }
                break;
            } else {
                if ( argFileName == null )
                    argFileName = s;
            }
        }

        if ( false ) {
            // dummy
            return null; 
        } else if ( argSpecialAllAvailableReferences ) {
            return outputAllAvailableReferences( argFileName );
        } else if ( argSpecialOutputReference ) {
            return outputReference( argFileName,  argSpecialOutputReferenceName );
        } else if ( argHttp || argGui ) {
            return start(argGui, argHttp, argHttpPort, argFileName);
        } else {
            System.err.println( "hmm... you have better to have at least one interface to control the system." );
            return start(argGui, argHttp, argHttpPort, argFileName);
        }
    }
    static void forceLoad( Class c ) {
        try {
            Class.forName( c.getName(), true, c.getClassLoader() );
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        }
    }
    
    static Collection<DescriptiveDocumentCategory> loadAllAvailableHelps() {
        forceLoad( PulsarDocuments.class );
        forceLoad( PulsarFramePackage.class );
        forceLoad( KawapadDocuments.class );
        forceLoad( DescriptiveHelp.class );
        
        return DescriptiveDocumentCategory.getAll();
    }
    
    static Pulsar outputAllAvailableReferences( String outputFile ) throws IOException {
        Pulsar pulsar = start(true,true,8193);
        List<String> stringList = new ArrayList<>();
        for ( DescriptiveDocumentCategory c : loadAllAvailableHelps() ) {
            stringList.add( SchemeUtils.schemeStringToJavaString( c.getSymbol() ) );
        }
        String str = String.join( ",", stringList );
        if ( outputFile == null /* || "-".equals( outputFile ) */ ) {
            System.out.println( str );      
        } else {
            FileOutputStream fo = null;
            try {
                fo = new FileOutputStream( new File( outputFile ) );
                fo.write( str.getBytes( Charset.forName( "utf-8" ) ) );
                System.err.println( str );      
                fo.flush();
            } finally {
                if ( fo != null )
                    fo.close();
            }
        }
        
        quitPulsarSafely( pulsar );
        return pulsar;
    }

    static Pulsar outputReference( String outputFile, String categoryName ) throws IOException {
        loadAllAvailableHelps();
        DescriptiveDocumentCategory category = DescriptiveDocumentCategory.valueOf( categoryName );
        Pulsar pulsar = start(true,true,8193);
        String str = DescriptiveHelp.outputMarkdownReference( category, pulsar.getSchemeSecretary() );
        if ( outputFile == null /* || "-".equals( outputFile ) */ ) {
            System.out.println( str );      
        } else {
            FileOutputStream fo = null;
            try {
                fo = new FileOutputStream( new File( outputFile ) );
                fo.write( str.getBytes( Charset.forName( "utf-8" ) ) );
                System.err.println( str );      
                fo.flush();
            } finally {
                if ( fo != null )
                    fo.close();
            }
        }

        quitPulsarSafely( pulsar );
        return pulsar;
    }
    static void quitPulsarSafely(Pulsar pulsar) {
        // Pause for the safe shutdown; at this moment, other initializers are still
        // working and they somehow remain on the AWT-eventqueue and prevent JVM to
        // shutdown. (Mon, 26 Aug 2019 14:11:31 +0900)
        try {
            Thread.sleep( 2048 );
        } catch (InterruptedException e) {
            System.err.println( e.getMessage() );
        }
        pulsar.invokeLater( new Runnable() {
            @Override
            public void run() {
                pulsar.quit();
            }
        });
        // This is possibly not necessary. But it might help to flush the AWT-eventqueue.
        // See https://stackoverflow.com/questions/6309407/remove-top-level-container-on-runtime
        System.gc();
    }
    
    public static Pulsar start( boolean guiEnabled, boolean httpEnabled, int httpPort, String filename ) throws IOException {
//      >>> VERSION 1 
//      this.schemeSecretary = new SchemeSecretary();
//      this.schemeSecretary.setDirectMeeting( false );
//      KawaPad.registerGlobalSchemeInitializer( schemeSecretary );
//      this.schemeSecretary.newScheme();
//
//      Pulsar.registerLocalSchemeInitializers( schemeSecretary, this );
//      Pulsar.invokeLocalSchemeInitializers( schemeSecretary, this );
//      <<< VERSION 1

        /*
         * Search INIT_02 inside the entire workspace to know the modification of the
         * order of Pulsar's initialization.
         */
//      >>> VERSION INIT_02 (Sat, 03 Aug 2019 15:47:41 +0900)
        SchemeSecretary schemeSecretary = new SchemeSecretary();
        schemeSecretary.setDirectMeeting( false );

        DescriptiveHelp.registerGlobalSchemeInitializer( schemeSecretary );
        PulsarFrame.registerGlobalSchemeInitializers( schemeSecretary );
        Pulsar pulsar = new Pulsar( schemeSecretary );

        if ( guiEnabled )
            Kawapad.registerGlobalSchemeInitializer( schemeSecretary );
        
        Pulsar.registerLocalSchemeInitializers( schemeSecretary, pulsar );
        Pulsar.registerFinalSchemeInitializers( schemeSecretary, pulsar );
        
        
//      <<< VERSION INIT_02 (Sat, 03 Aug 2019 15:47:41 +0900)
        
        PulsarFrame pulsarGui;
        if ( guiEnabled )
            pulsarGui = PulsarFrame.start( pulsar, true );
        else
            pulsarGui = null;
        
        @SuppressWarnings("unused")
        SchemeHttp schemeHttp;
        if ( httpEnabled )
            schemeHttp = new SchemeHttp( schemeSecretary, httpPort, 
                Arrays.asList( pulsarGui.frame.getKawapad().threadInitializer()));
        else
            schemeHttp = null;
        
//      REMOVED >>> INIT_02 (Sat, 03 Aug 2019 15:47:41 +0900)
//      setting enableTime should be done only in newScheme(); 
//      this.enabledTimer = true;
//      REMOVED <<< INIT_02 (Sat, 03 Aug 2019 15:47:41 +0900)
        
        schemeSecretary.newScheme();
        
        // INIT_03 : it appears that INIT_02 which is a initial correction of
        // the initializing order of pulsar/kawapad is not sufficient.
        // Initializing scheme objects and initializing frames should be separately
        // initialized.
        // 
        // The method init() is called whenever the frame is created.
        if ( pulsarGui != null )
            pulsarGui.init();

        if ( filename != null && pulsarGui != null )
            pulsarGui.openFile( new File( filename ) );
        else
            pulsarGui.openIntro();
        
        return pulsar;
    }
    public static Pulsar start(boolean guiEnabled, boolean httpEnabled, int httpPort ) throws IOException {
        return start(guiEnabled, httpEnabled, httpPort, null );
    }
    public static Pulsar start(boolean guiEnabled, boolean httpEnabled ) throws IOException {
        return start(guiEnabled, httpEnabled, 8192, null );
    }
    public static Pulsar start(boolean guiEnabled ) throws IOException {
        return start(guiEnabled, true, 8192, null );
    }
    public static Pulsar start() throws IOException {
        return start(true, true, 8192, null );
    }

    public static void main(String[] args) throws IOException {
        PulsarLogger.init();
        parseArgsAndStartPulsar(args);
    }

}
