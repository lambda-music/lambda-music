package pulsar;

import java.io.File;
import java.io.IOException;

import kawapad.KawaPad;
import pulsar.lib.PulsarLogger;
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
			} else {
				if ( argFileName == null )
					argFileName = s;
			}
		}

		if ( argHttp || argGui ) {
			return start(argGui, argHttp, argHttpPort, argFileName);
		} else {
			System.err.println( "hmm... you have better to have at least one interface to control the system." );
			return start(argGui, argHttp, argHttpPort, argFileName);
		}
	}
	public static Pulsar start( boolean guiEnabled, boolean httpEnabled, int httpPort, String filename ) throws IOException {
//		>>> VERSION 1 
//		this.schemeSecretary = new SchemeSecretary();
//		this.schemeSecretary.setDirectMeeting( false );
//		KawaPad.registerGlobalSchemeInitializer( schemeSecretary );
//		this.schemeSecretary.newScheme();
//
//		Pulsar.registerLocalSchemeInitializers( schemeSecretary, this );
//		Pulsar.invokeLocalSchemeInitializers( schemeSecretary, this );
//		<<< VERSION 1

		/*
		 * Search INIT_02 inside the entire workspace to know the modification of the
		 * order of Pulsar's initialization.
		 */
//		>>> VERSION INIT_02 (Sat, 03 Aug 2019 15:47:41 +0900)
		SchemeSecretary schemeSecretary = new SchemeSecretary();
		schemeSecretary.setDirectMeeting( false );

		Pulsar pulsar = new Pulsar( schemeSecretary );

		if ( guiEnabled )
			KawaPad.registerGlobalSchemeInitializer( schemeSecretary );
		
		Pulsar.registerLocalSchemeInitializers( schemeSecretary, pulsar );
//		<<< VERSION INIT_02 (Sat, 03 Aug 2019 15:47:41 +0900)
		
		PulsarGui pulsarGui;
		if ( guiEnabled )
			pulsarGui = PulsarGui.start( pulsar, true );
		else
			pulsarGui = null;
		
		@SuppressWarnings("unused")
		SchemeHttp schemeHttp;
		if ( httpEnabled )
			schemeHttp = new SchemeHttp( schemeSecretary, httpPort );
		else
			schemeHttp = null;
		
//		REMOVED >>> INIT_02 (Sat, 03 Aug 2019 15:47:41 +0900)
//		setting enableTime should be done only in newScheme(); 
//		this.enabledTimer = true;
//		REMOVED <<< INIT_02 (Sat, 03 Aug 2019 15:47:41 +0900)
		
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
