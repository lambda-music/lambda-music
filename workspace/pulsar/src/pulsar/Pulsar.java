/*
 * Pulsar-Sequencer written by Atsushi Oka 
 * Copyright 2018 Atsushi Oka
 *
 * This file is part of Pulsar-Sequencer. 
 * 
 * Pulsar-Sequencer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * Pulsar-Sequencer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with Pulsar-Sequencer.  If not, see <https://www.gnu.org/licenses/>.
 */

package pulsar;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.lang.invoke.MethodHandles;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import javax.swing.JComboBox;

import org.jaudiolibs.jnajack.JackException;

import gnu.expr.Keyword;
import gnu.lists.EmptyList;
import gnu.lists.IString;
import gnu.lists.LList;
import gnu.lists.Pair;
import gnu.mapping.Procedure;
import gnu.mapping.Procedure1;
import gnu.mapping.Symbol;
import gnu.mapping.Values;
import gnu.mapping.WrongArguments;
import gnu.math.DFloNum;
import kawa.standard.Scheme;
import metro.Metro;
import metro.MetroPort;
import metro.MetroTrack;
import metro.MetroTrack.SyncType;
import pulsar.lib.scheme.DProcedure0;
import pulsar.lib.scheme.DProcedure1;
import pulsar.lib.scheme.DProcedure3;
import pulsar.lib.scheme.DProcedureN;
import pulsar.lib.scheme.DescriptiveInitializerA;
import pulsar.lib.scheme.DescriptiveInitializerB;
import pulsar.lib.scheme.DescriptiveProcedure;
import pulsar.lib.scheme.SchemeUtils;
import pulsar.lib.scheme.scretary.SchemeSecretary;
import pulsar.lib.secretary.Invokable;
import pulsar.lib.secretary.InvokablyRunnable;
import pulsar.lib.secretary.SecretaryMessage;
import pulsar.lib.swing.MersenneTwisterFast;

/**
 * Pulsar is a MIDI sequencer program which is controlled by a powerful computer
 * language which is called Scheme. Pulsar is written in JAVA and uses JACK
 * Audio Connection Kit to receive and send MIDI messages; Pulsar runs on any
 * Linux boxes, hopefully OSX and Windows as well.
 * <p>
 * Pulsar dynamically composes music by writing programs by Scheme. Scheme is a
 * dialect of a computer language which is called "lisp". Pulsar could be used
 * to develop automatic DJ application or automatic backing track generators
 * which composes on the fly.
 * <h3>Workflow</h3>
 * <i>TODO</i>
 * <p>
 * <h3>Document Model</h3>
 * This application has a slightly different concept of open/close files
 * comparative to those general music players. When a user opens a scheme script
 * on Pulsar, Pulsar merely executes it without any clean ups nor
 * initializations. For example, if a user opens an empty scheme script while
 * Pulsar is playing music, nothing will be happened and keeps the former state.
 * <p>
 * In Pulsar, scripts should implicitly initialize the state of the sequencer if
 * it is necessary because Pulsar does not do it automatically.
 * <p>
 * Pulsar has two important parameters :
 * <p>
 * <ul>
 * <li>main-file
 * <li>main-invokable
 * </ul>
 * <p>
 * The <code>main-file</code> is a file path to the main file which Pulsar is
 * currently referring. Pulsar keeps checking the timestamp of the file and
 * trying to detect file modification. Whenever Pulsar detects any timestamp
 * update on the file, Pulsar automatically reads it and execute.
 * <p>
 * The <code>main-invokable</code> is the invokable which initializes the state
 * of the sequencer. This method is the place where Pulsar starts a new song.
 * <p>
 * If a script file sets <code>main-invokable</code>, this is effectively the
 * application <i>opens</i> a new-file in the sense of general applications. A
 * script file could also leave <code>main-invokable</code> untouched.
 * <p>
 * This behavior is designed to be useful in some scenarios. For example, when a
 * user repeatedly updates/modifies the script file repeatedly in order to check
 * how it sounds, this behavior might help the user. If Pulse initializes the
 * sequencer whenever it reads a file, the music must restart from the
 * beginning. Users usually prefer it to keep the music playing and modify the
 * music slightly to see what will happen.
 * <p>
 * This behavior could probably be used as implementing sub-modules.
 * <h3>Pulsar API<h3>
 * 
 * @author Atsushi Oka
 */
public final class Pulsar extends Metro {
	static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
	static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE,   msg, e   ); }
	static void logInfo (String msg             ) { LOGGER.log(Level.INFO,     msg      ); }
	static void logWarn (String msg             ) { LOGGER.log(Level.WARNING,  msg      ); }

	static String messageWarnIgnoredMissingSyncTrack( Object arg ) {
		return "could not find a track which name was " + arg + " ... ignored.";
	}
	
	long shutdownWait = 1024;

	public static void registerLocalSchemeInitializers( SchemeSecretary schemeSecretary, Pulsar pulsar ) {
		schemeSecretary.registerSchemeInitializer( pulsar, new SecretaryMessage.NoReturnNoThrow<Scheme>() {
			@Override
			public void execute0( Scheme scheme, Object[] args ) {
				/*
				 * INIT_02
				 * We give up to initialize current thread if it is not running.
				 * This might happen in initializing the object in the constructor method.
				 * This seems to be a problem, but it will be initialized when Metro is opened
				 * by open() method; therefore, we temporally give up the initializing process.  
				 */
				if ( pulsar.isOpened() ) {
					logWarn( "registerLocalSchemeInitializers_POSTMESSAGE  DONE" );
					pulsar.postMessage( new Runnable() {
						@Override
						public void run() {
							// 6. This initializes the thread of Metro's message-queue.
							// See pulsar.lib.secretary.scheme.SchemeSecretary#specialInit()
							SchemeSecretary.initializeSchemeForCurrentThreadStatic( scheme );
						}
					});
				} else {
					logWarn( "registerLocalSchemeInitializers_POSTMESSAGE IGNORED" );
				}
			}
		});
		schemeSecretary.registerSchemeInitializer( pulsar, new SecretaryMessage.NoReturnNoThrow<Scheme>() {
			@Override
			public void execute0( Scheme scheme, Object[] args ) {
				pulsar.initScheme( scheme );
			}
		});
		schemeSecretary.registerSchemeInitializer( pulsar, new SecretaryMessage.NoReturnNoThrow<Scheme>() {
			@Override
			public void execute0( Scheme scheme, Object[] args ) {
				PulsarGuiUtils.initStaticScheme( pulsar, scheme );
			}
		});

	}
	public static void invokeLocalSchemeInitializers( SchemeSecretary schemeSecretary, Pulsar pulsar ) {
		schemeSecretary.invokeSchemeInitializers( pulsar );
	}
	
	public static void registerFinalSchemeInitializers( SchemeSecretary schemeSecretary, Pulsar pulsar ) {
		schemeSecretary.registerSchemeInitializer( pulsar, new SecretaryMessage.NoReturnNoThrow<Scheme>() {
			@Override
			public void execute0( Scheme scheme, Object[] args ) {
				PulsarGui.addLispWords( scheme, PulsarGui.getPulsarWords(scheme) );
			}
		});
	}


	@Override
	protected void onCreateThread() {
		super.onCreateThread();
//		registerLocalSchemeInitializers( this.schemeSecretary, this );
	}
	
	/**
	 * Creates an instance of Pulsar object without opening any specific scheme
	 * file. When a user creates an object by this constructor, the sequencer
	 * remains closed after the application boots up. The user must explicitly
	 * open a file to use the application.
	 * @throws IOException 
	 */
	public Pulsar( SchemeSecretary schemeSecretary ) throws IOException {
		super();
		this.schemeSecretary = schemeSecretary;
	}

	
//	@Override
//	public void open(String clientName) throws JackException {
//		super.open(clientName);
//		newScheme();
//	}
	
//	PulsarGui pulsarGui;
//	SchemeHttp schemeHttp;

	boolean isQuitting = false;
	
	/**
	 * Notify every interface to shutdown the application. Then shutdown. 
	 */
	public void quit() {
		isQuitting = true;
		shutdown();
	}

    @Override
    public void close() {
    	super.close();
    }
    
    public void shutdown() {
		close();
		execCleanupHook();
    	getSchemeSecretary().executeShutdownHook();
    }

	/**
	 * reset() method resets the scheme environment.
	 */
	public void reset() {
		logInfo("===Pulsar.reset()");
		newScheme();
		execCleanupHook();
		close();
	}
	
	private final SchemeSecretary schemeSecretary;
	public SchemeSecretary getSchemeSecretary() {
		return schemeSecretary;
	}
	
	private void newScheme() {
		logInfo("Pulsar#newScheme() "); 
		this.getSchemeSecretary().newScheme();
	}

	public Invokable createInvokable( Procedure procedure ) {
//		return InvokableSchemeProcedure.createSecretariallyInvokable( schemeSecretary, procedure );
		return getSchemeSecretary().createSecretarillyInvokable( procedure );
	}
	public Invokable createInvokable2( Procedure procedure ) {
//		return InvokableSchemeProcedure.createSecretariallyInvokable( getSchemeSecretary(), procedure );
		return getSchemeSecretary().createSecretarillyInvokable( procedure );
	}
	public Runnable createRunnableAndInvocable( Procedure procedure, Object... args) {
		return new InvokablyRunnable( getSchemeSecretary().createSecretarillyInvokable( procedure ), args );
	}

//	public static InvokableSchemeProcedure createInvocable(
//			Procedure invokable) {
//		return new InvokableSchemeProcedure(syncObj, environment, language, invokable);
//	}

	
	MersenneTwisterFast random = new MersenneTwisterFast( new int[] { 
			(int) System.currentTimeMillis(),
			0x123, 0x234, 0x345, 0x456,
	});
	
	
	/**
	 * This field specifies the invokable to reset all of the states inside the
	 * sequencer and effectively this method starts a song. Whenever a user call
	 * {@link Pulsar#rewind()}, this invokable will be invoked.
	 */
	transient Invokable mainProcedure = null;

	/**
	 * Sets the main-invokable object.
	 * 
	 * @see Pulsar#mainProcedure 
	 */
	public void setMainProcedure( Invokable mainProcedure ) {
		this.mainProcedure = mainProcedure;
	}
	
	/**
	 * Returns the main-invokable object.
	 * 
	 * @see Pulsar#mainProcedure 
	 */
	public Invokable getMainProcedure() {
		return mainProcedure;
	}
	

	
	
    /**
	 * {@link Pulsar#rewind()} method resets the state of the object and calls main
	 * invokable to back to the state of beginning of the project. This method:9
	 * effectively invoke the main invokable. See {@link Pulsar#mainProcedure}
	 */
    public void rewind() { 
    	logInfo( "===rewind" );
    	setPlaying(false);
    	clearTracks();
    	if ( mainProcedure != null )
    		mainProcedure.invoke();
    }

    /**
     * This hook objects will be invoked whenever reset() method is called.
     */
    final Collection<Runnable> cleanupHook = new LinkedList<>();
    
    /**
     * Add a hook that will be invoked whenever reset() method is called.
     */
    public void addCleanupHook( Runnable runnable ) {
    	synchronized ( cleanupHook ) { 
    		cleanupHook.add( runnable );
    	}
    }
    public void execCleanupHook( ) {
    	synchronized ( cleanupHook ) {
    		for ( Iterator<Runnable> i =cleanupHook.iterator(); i.hasNext(); ) {
    			Runnable runnable = i.next();
    			try {
    				runnable.run();
    			} catch ( Throwable e ) {
    				logError("CLEANUP HOOK: IGNORED AN EXCEPTION", e);
    			}
    		}
    		cleanupHook.clear();
    	}
    }
    

	public File getConfigDir() {
		final File configDir = new File( System.getProperty("user.home"), ".pulsar" );
		if ( ! configDir.isDirectory() ) {
			if (! configDir.mkdir() ) {
				logInfo( "WARNING : Failed to create the config directory." );
			}
		}
		return configDir;
	}

	public File getConfigFile() {
		// Configuration Directory
		final File configFile = new File( getConfigDir(), "init.scm" );
		if ( ! configFile.isFile() ) {
			try {
				configFile.createNewFile();
			} catch (IOException e) {
				logInfo( "WARNING : Failed to create the main config file." );
				e.printStackTrace();
			}
		}
		return configFile;
	}

	/**
	 * This mechanism is currently not used. See {@link #readHistoryFile(JComboBox)}
	 * @return
	 */
	public File getHistoryFile() {
		// Configuration Directory
		final File historyFile = new File( getConfigDir(), "history.txt" );
		if ( ! historyFile.isFile() ) {
			try {
				historyFile.createNewFile();
			} catch (IOException e) {
				logError( "WARNING : Failed to create the history file.", e );
			}
		}
		return historyFile;
	}

    /**
     * Currently this method is not used. 
     * @param comboBox
     */
	public void readHistoryFile(JComboBox<String> comboBox) {
		comboBox.removeAllItems();

		BufferedReader in = null;
		try {
			in = new BufferedReader( new FileReader( getHistoryFile() ) );
			String s = null;
			for (;;) {
				s = in.readLine();
				if ( s == null ) break;
				// if ( new File(s).isFile() ) {
					comboBox.addItem( s );
				// }
			}
		} catch (FileNotFoundException e1) {
			logError( "" , e1 );
		} catch (IOException e1) {
			logError( "" , e1 );
		} finally {
			if ( in != null )
				try {
					in.close();
				} catch (IOException e1) {
					logError( "" , e1 );
				}
		}
	}
	final File configFile = getConfigFile();
	
	

	public static Runnable createTimer( Pulsar pulsar, long delay, long interval, Invokable invokable ) {
		java.util.Timer timer = new java.util.Timer( true );
		timer.scheduleAtFixedRate( new java.util.TimerTask() {
			@Override
			public void run() {
				// The life cycle of timer threads may not be able to be controlled by users;
				// therefore, we decided to initialize the environment every time we execute
				// timer events. (Mon, 05 Aug 2019 00:38:14 +0900)
				pulsar.getSchemeSecretary().initializeSchemeForCurrentThread();
				
				// Execute the specified process.
				Object result = invokable.invoke();
				if ( Boolean.FALSE.equals( result ) ) {
					timer.cancel();
				}
			}
		}, delay, interval );

		pulsar.addCleanupHook( new Runnable() {
			@Override
			public void run() {
				timer.cancel();
			}
		});
		
		return new Runnable() {
			@Override
			public void run() {
				timer.cancel();
			}
		};
	}

	private static final class TagSearchUserProcedure extends DProcedure1 {
		private final Procedure proc;
		private final Object p;
		TagSearchUserProcedure(Object p, Procedure proc) {
			this.p = p;
			this.proc = proc;
		}
		@Override
		public Object apply1(Object arg1) throws Throwable {
			return proc.apply2( arg1, p );
		}
	}
	private static final class TagSearchAndProcedure extends DProcedure1 {
		private final Pair p;
		TagSearchAndProcedure(Pair p) {
			this.p = p;
		}
		@Override
		public Object apply1(Object arg1) throws Throwable {
			Collection p2 = (Collection)arg1;
			return p2.containsAll( p );
		}
	}
	private static final class TagSearchOrProcedure extends DProcedure1 {
		private final Pair p;
		TagSearchOrProcedure(Pair p) {
			this.p = p;
		}
		@Override
		public Object apply1(Object arg1) throws Throwable {
			Collection p2 = (Collection)arg1;
			for ( Object o : p ) {
				if ( p2.contains(o) ) 
					return true;
			}
			return false;
		}
	}
	private static final class TagSearchIsProcedure extends DProcedure1 {
		private final Object value;
		TagSearchIsProcedure(Object value) {
			this.value = value;
		}
		@Override
		public Object apply1(Object arg1) throws Throwable {
			return value.equals( arg1 );
		}
	}

	final static class TrackProcedure extends DProcedure0 {
		final Pair pair;
		TrackProcedure( Pair pair ) {
			this.pair = pair;
		}

		@Override
		public Object apply0() throws Throwable {
			return pair;
		}
	}
	interface TempoTapperTempoNotifier {
		void notifyTempo( double beatPerMinute );
	}
	class TempoTapper {
		final List<TempoTapperTempoNotifier> notifiers = new ArrayList<>();
		void registerNotifier( TempoTapperTempoNotifier notifier ) {
			notifiers.add( notifier );
		}
		
		long prev_time = 0;
		int BUF_SIZE = 3;
		long TIMEOUT = 1000L*1000L*1000L*2L;
		void reset() {
			for ( int i=0;i<t.length; i++ )
				t[i]=0;
			tidx=0;
		}
		long t[] = new long[BUF_SIZE];
		int tidx= 0;
		
		public void tap( ) {
			long current_time = System.nanoTime();
			if ( prev_time == 0 ) {
				prev_time = current_time;
				return;
			}
			if ( TIMEOUT < current_time - prev_time ) {
				prev_time = current_time;
				reset();
				return;
			}

			long current_diff = current_time - prev_time ;
			logInfo( "Elapsed Time : " + current_diff );

			tidx ++;
			if( tidx < t.length ) {
			} else {
				tidx = 0;
			}

			t[tidx] = current_diff;
			prev_time = current_time;

			boolean isFull = true;
			long sum = 0;
			{
				for ( int i=0; i<t.length; i++ ) {
					if ( 0 < t[i] ) {
						sum += t[i];
					} else {
						isFull = false;
						break;
					}
				}
			}

			if ( isFull ) 
				try {
					double avg = (double)sum / t.length;
					double onemin = 1000L*1000L*1000L*60L;
					double beatsPerMinute =  onemin / avg  ;
					
					double currentBeatsPerMinute = getBeatsPerMinute();
					beatsPerMinute = ( beatsPerMinute + currentBeatsPerMinute * 2 ) / 3;
					logInfo( String.format( "%.2f / %.2f = %.2f", onemin , avg , beatsPerMinute  ) );
					
					setBeatsPerMinute( beatsPerMinute );
					
				} catch (JackException e1) {
					logError("", e1);
				}
		}

		public void setBeatsPerMinute(double beatsPerMinute) throws JackException {
			Pulsar.this.setBeatsPerMinute( (long) beatsPerMinute );
			for ( TempoTapperTempoNotifier n : notifiers ) {
				n.notifyTempo( beatsPerMinute );
			}
		}
	}
	final TempoTapper tempoTapper = new TempoTapper();


	/**
	 * Loads and executes the specified scheme script file.
	 * 
	 * If the specified path is a relative path, {@link Pulsar#loadScheme(File) }
	 * try to resolve the path from the {@link Pulsar#parentFile}. If {@link Pulsar#parentFile}
	 * is not specified, {@link Pulsar#loadScheme(File) } throws an exception.
	 * 
	 * @param file
	 * @throws FileNotFoundException
	 */
	@Deprecated
	public void loadScheme( File file ) throws FileNotFoundException {
		getSchemeSecretary().executeSecretarially(
			new SecretaryMessage.NoReturn<Scheme,FileNotFoundException>() {
				@Override
				public void execute0( Scheme scheme, Object[] args ) throws FileNotFoundException {
					SchemeUtils.execSchemeFromFile( scheme, file );
				}
			});
	}

	interface ConnectProc {
		void apply( Pulsar pulsar, String from, String to ) throws JackException;
		ConnectProc CONNECT = new ConnectProc() {
			@Override
			public void apply(Pulsar pulsar, String from, String to) throws JackException {
				pulsar.connectPort(from, to);
			}
		};
		ConnectProc DISCONNECT = new ConnectProc() {
			@Override
			public void apply(Pulsar pulsar, String from, String to) throws JackException {
				pulsar.disconnectPort(from, to);
			}
		};
	}
	
	static void connectProc(Pulsar pulsar, Object[] args, ConnectProc proc ) {
		ArrayDeque<Object> deque = new ArrayDeque<>( Arrays.asList( args ) );
		while ( 0 < deque.size() ) {
			Object fromObj = deque.pop();
			Object toObj = deque.pop();
			if ( fromObj == null || toObj == null ) {
				break;
			}
			String from = fromObj != null ? SchemeUtils.toString( fromObj ) : null;
			String to   = toObj   != null ? SchemeUtils.toString( toObj   ) : null;
			try {
				proc.apply(pulsar, from, to );
			} catch ( JackException e  ) {
				logError( "" , e );
			}
		}
	}

	public MetroTrack createTrack( Object name, Collection<Object> tags, Procedure procedure ) {
		return createTrack( name, tags, new SchemeSequence( createInvokable( procedure ) ) );
	}

	/**
	 *  
	 * @param object
	 * @return
	 *    a newly created list which can safely be modified.
	 */
	static List<Object> readParamTrackName( Object object ) { 
		object = SchemeUtils.schemeNullCheck( object );
		
		if ( object instanceof Pair ) {
			return new ArrayList<>( (Pair)object );
		} else {
			return new ArrayList<>( Arrays.asList( SchemeUtils.schemeNullCheck( object ) ) );
		}
	}
	
	/**
	 * XXX 
	 * 
	 * @param object
	 * @return
	 */
	static Procedure readParamTrackSearcher( Object object ) { 
		object = SchemeUtils.schemeNullCheck( object );
		
		// TAG SEARCH
		if ( object instanceof Procedure ) {
			return (Procedure) object;
		} else if ( object instanceof Symbol || object instanceof IString ) {
			return new TagSearchIsProcedure(object);
		} else if ( object instanceof Pair ) {
			// TODO DOCUMENT THIS
			Object car = ((Pair)object).getCar();
			Object cdr = ((Pair)object).getCdr(); // <<< FIXED (Tue, 20 Aug 2019 22:27:46 +0900)
			if ( car instanceof Procedure ) {
				return new TagSearchUserProcedure(cdr, (Procedure) car ); 
			} else if ( car instanceof Keyword ) {
				switch ( ((Keyword)car).getName() ) {
					case "and" :
						return new TagSearchAndProcedure((Pair) object );
					case "or" :
						return new TagSearchOrProcedure((Pair) object );
					default : 
						throw new IllegalArgumentException( "The keyword " + car + " is not allowed here." );
				}
			} else {
				return new TagSearchAndProcedure((Pair) object );
			}
		} else {
			throw new IllegalArgumentException();
		}
	}
	List<MetroTrack> readParamTrack( Object object ) {
		if ( object instanceof Pair ) {
			if (((Pair)object).getCar() instanceof MetroTrack) {
				return (List<MetroTrack>) object;
			} else if ( NoteListParser.isNotationList(object) ) {
				return Arrays.asList( createTrack( null, null, new TrackProcedure( (Pair) object ) ) );
			} else {
				/// XXX ??? (Tue, 13 Aug 2019 00:43:25 +0900)
				return searchTrack( 
					createInvokable( 
						readParamTrackSearcher( object )));				
//				throw new IllegalArgumentException( "The value '" + object + "' is not allowed here." );
			}
		} else if ( object instanceof MetroTrack ) {
			return Arrays.<MetroTrack>asList((MetroTrack)object);
		} else if ( object instanceof EmptyList ) {
			return Collections.EMPTY_LIST;
		} else {
			return searchTrack( 
				createInvokable( 
					readParamTrackSearcher( object )));				

//			throw new IllegalArgumentException( "The value '" + object + "' is not allowed here." );
		}
	}
	static double readParamSyncOffset(Object object) {
		return SchemeUtils.toDouble( object );
	}
	static SyncType readParamSyncType(Object object) {
		object = SchemeUtils.schemeNullCheck(object);
		if ( object == null ) {
			return SyncType.IMMEDIATE;
		} else {
			return SyncType.toSyncType( SchemeUtils.toString( object ) );
		}
	}
	static Procedure readParamProcedure(Object arg) {
		if ( arg  == null ) {
			return null;
		} else if ( arg instanceof Procedure ) {
			return (Procedure) arg;
		} else if ( arg  instanceof Pair ) {
			return new TrackProcedure((Pair)arg);
		} else {
			throw new IllegalArgumentException( "unsupported type of the argument" );
		}
	}

	protected List<Object> readParamPortName( Object arg ) {
		if ( arg instanceof Pair ) {
			return ((Pair)arg);
		} else {
			return Arrays.asList( arg );
		}
	}
	protected List<MetroPort> readParamPort( Object arg ) {
		if ( arg instanceof Pair ) {
			List<MetroPort> list = new ArrayList<>();
			for ( Object o : ((Pair)arg) ) {
				list.addAll( readParamPort( o ) );
			}
			return list;
		} else if ( arg instanceof MetroPort ) {
			return Arrays.asList( (MetroPort)arg );
		} else if ( arg instanceof IString || arg instanceof Symbol ) {
			MetroPort port = readParamNameToPort( getInputPorts(), arg );
			if ( port == null ) {
				logWarn( "unsupported type of a value (" + arg + ")" );
				return Collections.EMPTY_LIST;
			} else {
				return Arrays.asList( port );
			}
		} else {
			logWarn( "unsupported type of a value (" + arg + ")" );
			return Collections.EMPTY_LIST;
		}
	}
	
	private MetroPort readParamNameToPort( List<MetroPort> portList, Object arg ) {
		if ( arg instanceof MetroPort ) 
			return (MetroPort)arg;
		
		for ( MetroPort p : portList ) {
			if ( p.getName().equals( arg ) ) 
				return p;
		}
		return null;
	}
	
	private static final String THROWS_AN_ERROR_IF_NOT_OPEN = 
			"In case the current sequencer system has not established any connection to the JACK, " + 
			"it throws an exception. ";

	private static final String ALTERS_THE_CURRENT_STATE =
			"This procedure alters the current sequencer system's state. ";


	
	/**
	 * Initializes an environment of scheme engine and defines API for the scripts.
	 * 
	 * @param scheme
	 *            the scheme instance to initialize.
	 */
	public void initScheme( Scheme scheme ) {
		SchemeUtils.defineVar( scheme, this , "pulsar" );
		SchemeUtils.defineVar( scheme, new DProcedureN( "open?" ) {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				return isOpened();
			}
		} , "open?");
		
		SchemeUtils.setDocumentInitializer( scheme,  
			new DescriptiveInitializerA() {{
				setParameterDescription(   "" );
				setReturnValueDescription( "::boolean" );
				setShortDescription(       "returns the current open state. " );
				setLongDescription(        "This procedure returns #t iff the current sequencer state is open; "
						                 + "otherwise returns #f. " );
			}},
			new DescriptiveInitializerB(),
			"open?" );
		
		SchemeUtils.defineVar( scheme, new DProcedure1("open") {
			@Override
			public Object apply1(Object arg0) throws Throwable {
				open( SchemeUtils.toString( arg0 ) );
				return Invokable.NO_RESULT;
			}
		} , "open");
		
		SchemeUtils.setDocumentInitializer( scheme,  
			new DescriptiveInitializerA() {{
				setParameterDescription( "[string]" );
				setReturnValueDescription( "::void" );
				setShortDescription( "starts a new connection between JACK Audio Connection Kit." );
				setLongDescription( 
						  "This procedure opens a new connection to the installed JACK Audio Connection Kit with"
						+ "the specified port name. "
						+ "When it failed to open a connection, this throws an exception. "
						+ ALTERS_THE_CURRENT_STATE );
			}},
			new DescriptiveInitializerB(),
			"open" );

		
		SchemeUtils.defineVar( scheme, new DProcedureN("close") {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				close();
				return Invokable.NO_RESULT;
			}
		} , "close" );

		SchemeUtils.setDocumentInitializer( scheme,  
			new DescriptiveInitializerA() {{
				setParameterDescription( "" );
				setReturnValueDescription( "::void" );
				setShortDescription( "ends the current connection between JACK Audio Connection Kit." );
				setLongDescription( "This procedure closes the current connection to the JACK Audio Connection Kit. "
						+ "When it failed to close the connection, this throws an exception. "
						+ THROWS_AN_ERROR_IF_NOT_OPEN
						+ ALTERS_THE_CURRENT_STATE );
			}},
			"close" );

		//////////////////////////////////////////////////////////

		class InitDocOpenPorts extends DescriptiveInitializerA {
			@Override
			public String getParameterDescription() {
				return "[ANY|(ANY...)  ... ]";
			}
			@Override
			public String getReturnValueDescription() {
				return "::MetroPort";
			}
			@Override
			public String getShortDescription() {
				return "opens %s ports on the current JACK connection. ";
			}
			@Override
			public String getLongDescription() {
				return ""
						+"Each argument is the name of a port to create. "
						+ "The value can be a value of any type; thought, it is usually a value "
						+ "which is easy to be distinguished such as a symbol value or a string value. "
						+ "The value is applied as the identifier of the created port. "
						+ "A duplicated port name on the current JACK connection causes an exception to be thrown. "
						+ THROWS_AN_ERROR_IF_NOT_OPEN
						+ ALTERS_THE_CURRENT_STATE;
			};  
		}
		InitDocOpenPorts initDocOpenPorts = new InitDocOpenPorts() {{
				setParameterDescription( "[ANY|(ANY...)  ... ]" );
				setReturnValueDescription( "::MetroPort" );
				setShortDescription( "opens %s ports on the current JACK connection. " );
				setLongDescription( "" + "Each argument is the name of a port to create. "
				        + "The value can be a value of any type; thought, it is usually a value "
				        + "which is easy to be distinguished such as a symbol value or a string value. "
				        + "The value is applied as the identifier of the created port. "
				        + "A duplicated port name on the current JACK connection causes an exception to be thrown. "
				        + THROWS_AN_ERROR_IF_NOT_OPEN + ALTERS_THE_CURRENT_STATE
			);
		}};		
		
		DProcedureN openOutput = new DProcedureN("open-output") {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				ArrayList<MetroPort> list = new ArrayList<>();
				for ( Object o : args ) {
					for ( Object portName : readParamPortName( o ) ) {
						list.add( createOutputPort(portName) );
					}
				}
				Collections.reverse( list );
				return LList.makeList( list );
			}
		};
		SchemeUtils.defineVar( scheme, openOutput , "open-output"
												  , "openo"
												  , "output" );

		SchemeUtils.setDocumentInitializer( scheme, 
			initDocOpenPorts, 
			new DescriptiveInitializerB( "output" ),
		    "open-output" );
		
		//////////////////////////////////////////////////////////

		DProcedureN openInput = new DProcedureN("open-input") {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				ArrayList<MetroPort> list = new ArrayList<>();
				for ( Object o : args ) {
					for ( Object portName : readParamPortName( o ) ) {
						list.add( createInputPort(portName) );
					}
				}
				Collections.reverse( list );
				return LList.makeList( list );
			}
		};
		SchemeUtils.defineVar( scheme, openInput , "open-input"
												 , "openi"
												 , "input" );

		SchemeUtils.setDocumentInitializer( scheme, 
			initDocOpenPorts, 
			new DescriptiveInitializerB( "input" ),
		    "open-input" );

		
		//////////////////////////////////////////////////////////
		
		class InitDocClosePorts extends DescriptiveInitializerA {{
			setParameterDescription( "[MetroPort|symbol|string|(MetroPort|symbol|string ...) ... ]" );
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
		
		DProcedureN closeOutput = new DProcedureN("close-output") {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				for ( Object o : args ) {
					for ( MetroPort p : readParamPort( o ) ) {
					    destroyOutputPort( p );
					}
				}
				return Invokable.NO_RESULT;
			}
		};
		SchemeUtils.defineVar( scheme, closeOutput, "close-output"
												  , "closeo" );
		SchemeUtils.setDocumentInitializer( scheme, 
			initDocClosePorts, 
			new DescriptiveInitializerB( "output" ),
				"close-output" );

		//////////////////////////////////////////////////////////
		
		DProcedureN closeInput = new DProcedureN("close-input") {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				for ( Object o : args ) {
					for ( MetroPort p : readParamPort( o ) ) {
						destroyInputPort( p );
					}
				}
				return Invokable.NO_RESULT;
			}
		};
		SchemeUtils.defineVar( scheme, closeInput, "close-input","closei"  );
		
		SchemeUtils.setDocumentInitializer( scheme, 
			initDocClosePorts, 
			new DescriptiveInitializerB( "input" ),
			"close-input" );

		//////////////////////////////////////////////////////////

		class InitDocListPorts extends DescriptiveInitializerA {{
				setParameterDescription( "" );
				setReturnValueDescription( "::(MetroPort ...)" );
				setShortDescription( "returns a list which contains all %s ports on the current JACK connection. " );
				setLongDescription( ""
									+ "Each element on the list is a reference to a MetroPort object. "
									+ "The values in the list are sorted from newest to oldest. "
									+ THROWS_AN_ERROR_IF_NOT_OPEN
									);
		}}
		InitDocListPorts initDocListPorts = new InitDocListPorts();
 
		DProcedureN listOutput = new DProcedureN("list-output") {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				List<MetroPort> list = getOutputPorts();
				Collections.reverse( list );
				return LList.makeList( list  );
			}
		};
		SchemeUtils.defineVar( scheme, listOutput, "list-output"
												 , "lso" );
		SchemeUtils.setDocumentInitializer( scheme, 
			initDocListPorts, 
			new DescriptiveInitializerB( "output" ),
			"list-output" );
		
		
		//////////////////////////////////////////////////////////

		DProcedureN listInput = new DProcedureN("list-input") {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				List<MetroPort> list = getInputPorts();
				Collections.reverse( list );
				return LList.makeList( list  );
			}
		};
		SchemeUtils.defineVar( scheme, listInput, "list-input"
												, "lsi" );
		SchemeUtils.setDocumentInitializer( scheme, 
			initDocListPorts, 
			new DescriptiveInitializerB( "input" ),
			"list-input" );

		//////////////////////////////////////////////////////////

		class InitDocConnection extends DescriptiveInitializerA {{
				setParameterDescription( "[ANY ...]" );
				setReturnValueDescription( "::void" );
				setShortDescription( "%s specified two ports on the current JACK connection. " );
				setLongDescription( ""
									+ "This procedure %s "
									+ "the port which is specified in the first arguments to "
									+ "the port which is specified in the second arguments. "
									+ "The rest arguments are also processed in the same mannar. "
									+ THROWS_AN_ERROR_IF_NOT_OPEN );
		}}
		InitDocConnection initDocConnection = new InitDocConnection();
		
		SchemeUtils.defineVar( scheme, new DProcedureN("connect") {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				connectProc(Pulsar.this, args, ConnectProc.CONNECT );
				return Invokable.NO_RESULT;
			}
		} , "connect");
		
		SchemeUtils.setDocumentInitializer( scheme, 
			initDocConnection, 
			new DescriptiveInitializerB( "connects" ),
			"connect" );
		
		//////////////////////////////////////////////////////////
		
		SchemeUtils.defineVar( scheme, new DProcedureN("disconnect") {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				connectProc(Pulsar.this, args, ConnectProc.DISCONNECT );
				return Invokable.NO_RESULT;
			}
		} , "disconnect");
		SchemeUtils.setDocumentInitializer( scheme, 
			initDocConnection, 
			new DescriptiveInitializerB( "disconnects" ),
			"disconnect" );

		//////////////////////////////////////////////////////////
		
		class InitDocAllConnection extends DescriptiveInitializerA {{
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
		SchemeUtils.defineVar( scheme, new DProcedureN("get-all-output") {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				return Pair.makeList( getAllOutputPorts().stream().map( (v)->SchemeUtils.toSchemeString(v) )
					.collect( Collectors.toList() ) );
			}
		} , "get-all-output", "gao" );
		SchemeUtils.setDocumentInitializer( scheme, 
			initDocAllConnection, 
			new DescriptiveInitializerB( "output" ),
			"get-all-output" );

		//////////////////////////////////////////////////////////
		
		SchemeUtils.defineVar( scheme, new DProcedureN("get-all-input") {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				return Pair.makeList( getAllInputPorts().stream().map( (v)->SchemeUtils.toSchemeString(v) )
					.collect( Collectors.toList() ) );
			}
		} , "get-all-input", "gai");
		
		SchemeUtils.setDocumentInitializer( scheme, 
			initDocAllConnection, 
			new DescriptiveInitializerB( "input" ),
			"get-all-input" );

		//////////////////////////////////////////////////////////
		
		class InitDocSetGetMain extends DescriptiveInitializerA {{
				setParameterDescription( "[procedure]" );
				setReturnValueDescription( "::void" );
				setShortDescription( "%s the main procedure. " );
				setLongDescription( ""
									+ "The main procedure is a procedure which is called "
									+ "when (rewind) procedure is called in order to reset the sequencer's state. "
									+ "Usually, the main procedure is a procedure to boot up the current song system. "
									 );
			}}
		InitDocSetGetMain initDocMain= new InitDocSetGetMain();		 

		SchemeUtils.defineVar( scheme, new DProcedureN("set-main") {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				logInfo("set-main");
				if ( args.length == 1 ) {
					Procedure procedure = (Procedure)args[0];
					setMainProcedure( Pulsar.this.createInvokable(procedure) );
				} else {
					throw new RuntimeException( "invalid argument length" );
				}
				return Invokable.NO_RESULT;
			}
		} , "set-main");
		SchemeUtils.setDocumentInitializer( scheme, initDocMain,
			new DescriptiveInitializerB( "sets" ),
			"set-main" );

		//////////////////////////////////////////////////////////

		SchemeUtils.defineVar( scheme, new DProcedure0( "get-main" ) {
			@Override
			public Object apply0() throws Throwable {
				return getMainProcedure();
			}
		} , "get-main");
		SchemeUtils.setDocumentInitializer( scheme,
			initDocMain, 
			new DescriptiveInitializerB( "retrieves" ), 
			"get-main" );

		//////////////////////////////////////////////////////////
		
		SchemeUtils.defineVar( scheme, new DProcedureN("set-playing") {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				if ( args.length == 0 ) {
					togglePlaying();
				} else if ( args.length == 1 ) {
					setPlaying( (Boolean)args[0] );
				} else {
					throw new RuntimeException( "invalid argument length" );
				}
				return Invokable.NO_RESULT;
			}
		} , "set-playing");
		SchemeUtils.setDocumentInitializer( scheme,
			new DescriptiveInitializerA(){{
				setParameterDescription( "[boolean]" );
				setReturnValueDescription( "::void" );
				setShortDescription( "sets the current playing state." );
				setLongDescription( ""
									+ "When #f is passed to this procedure, the sequencer stops playing.  "
									+ "When #t is passed to this procedure ,the sequencer resumes playing. "
									+ THROWS_AN_ERROR_IF_NOT_OPEN
									 );
			}}, 
			"set-playing" );

		SchemeUtils.defineVar( scheme, new DProcedureN("playing?") {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				return getPlaying();
			}
		} , "playing?");
		SchemeUtils.setDocumentInitializer( scheme,
			new DescriptiveInitializerA(){{
				setParameterDescription( "" );
				setReturnValueDescription( "::boolean" );
				setShortDescription( "retrieves the current playing state." );
				setLongDescription( ""
									+ "When the sequencer is playing, it returns #t; otherwise it returns #f. "
									+ THROWS_AN_ERROR_IF_NOT_OPEN
									 );
			}}, 
			"playing?" );
		
		SchemeUtils.defineVar( scheme, new DProcedureN("play") {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				setPlaying( true ); 
				return Invokable.NO_RESULT;
			}
		} , "play");
		
		SchemeUtils.setDocumentInitializer( scheme,
			new DescriptiveInitializerA(){{
				setParameterDescription( "" );
				setReturnValueDescription( "::boolean" );
				setShortDescription( "causes the sequencer to start playing." );
				setLongDescription( ""
									+ THROWS_AN_ERROR_IF_NOT_OPEN
									 );
			}}, 
			"play" );

		SchemeUtils.defineVar( scheme, new DProcedureN("stop") {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				setPlaying( false ); 
				return Invokable.NO_RESULT;
			}
		} , "stop");
		SchemeUtils.setDocumentInitializer( scheme,
			new DescriptiveInitializerA(){{
				setParameterDescription( "" );
				setReturnValueDescription( "::boolean" );
				setShortDescription( "causes the sequencer to stop playing." );
				setLongDescription( ""
									+ THROWS_AN_ERROR_IF_NOT_OPEN
									 );
			}}, 
			"stop" );

		SchemeUtils.defineVar( scheme, new DProcedureN("quit") {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				Thread t = new Thread() {
					@Override
					public void run() {
						schemeSecretary.executeSecretarially( new SecretaryMessage.NoReturnNoThrow<Scheme>() {
							@Override
							public void execute0(Scheme resource, Object[] args) {
								try {
									Thread.sleep( shutdownWait );
								} catch (InterruptedException e) {
									logWarn( e.getMessage() );
								}
								quit(); 
							}
						}, Invokable.NOARG );
					}
				};
				t.start();
				
				return "Now Pulsar will shutdown in " + shutdownWait + " milliseconds...";
			}
			{
				setShortDescription( "Quit the application." );
				setLongDescription( "Quit the application." );
			}
		
		} , "quit");
		
		SchemeUtils.setDocumentInitializer( scheme,
			new DescriptiveInitializerA(){{
				setParameterDescription( "" );
				setReturnValueDescription( "::void" );
				setShortDescription( "makes the sequencer to stop playing "
					               	+ "and shutdowns the application in " + shutdownWait + " milliseconds. " );
				setLongDescription( ""
									+ "Currently the time to shutdown is hard-coded and cannot be changed. "
									+ THROWS_AN_ERROR_IF_NOT_OPEN );
			}}, 
			"stop" );
		
		SchemeUtils.defineVar( scheme, new DProcedureN("tap-tempo") {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				tempoTapper.tap(); 
				return Invokable.NO_RESULT;
			}
		} , "tap-tempo", "tapt");
		
		SchemeUtils.setDocumentInitializer( scheme,
			new DescriptiveInitializerA(){{
				setParameterDescription( "" );
				setReturnValueDescription( "::void" );
				setShortDescription( "has the same effect with pressing the tap-tempo button on the main screen. "
					               	);
				setLongDescription( ""
									+ "The tap-tempo button is a button to change the current tempo. "
									+ "The button is supposed to be pressed repeatedly to tell the system  "
									+ "how fast the sequencer should play the current music. "
									+ "The sequencer calculates the average of durations between the pressing the button, "
									+ "and apply the value as the current tempo on the sequencer system. "
									+ "See (help set-tempo)."
									+ THROWS_AN_ERROR_IF_NOT_OPEN );
			}}, 
			"tap-tempo" );

		SchemeUtils.defineVar( scheme, new DProcedureN("set-tempo") {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				if ( 0 < args.length ) {
					double bpm = SchemeUtils.toDouble(args[0]);
					tempoTapper.setBeatsPerMinute( bpm );
				}

				return Invokable.NO_RESULT;
			}
		} , "set-tempo" );
		
		SchemeUtils.setDocumentInitializer( scheme,
			new DescriptiveInitializerA(){{
				setParameterDescription( "[number]" );
				setReturnValueDescription( "::void" );
				setShortDescription( "sets the current tempo. "
					               	);
				setLongDescription( ""
									+ "This procedure takes an argument as a beat-per-minutes. "
									+ "A value less than one is treated as one. "
									+ "There is no maximum value for the argument. "
									+ "thought, the result of passing extreme values to the procedure "
									+ "is not defined. "
									+ "See (help tap-tempo)."
									+ THROWS_AN_ERROR_IF_NOT_OPEN );
			}}, 
			"set-tempo" );
		
		//////////////////////////////////////////////////////////

		/**
		 * This function only reset the current scheme environment.
		 * See {@link Pulsar#reset }
		 */
		DProcedure0 resetScheme = new DProcedure0( "reset" ) {
			@Override
			public Object apply0() throws Throwable {
				reset();
				return Invokable.NO_RESULT;
			}
		};
		SchemeUtils.defineVar( scheme, resetScheme , "reset" );
		SchemeUtils.setDocumentInitializer( scheme,
			new DescriptiveInitializerA(){{
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
			}}, 
			"reset" );
		
		
		SchemeUtils.defineVar( scheme, new DProcedure0("rewind") {
			@Override
			public Object apply0() throws Throwable {
				rewind();
				return Invokable.NO_RESULT;
			}
		} , "rewind");
		
		SchemeUtils.setDocumentInitializer( scheme,
			new DescriptiveInitializerA(){{
				setParameterDescription( "" );
				setReturnValueDescription( "::void" );
				setShortDescription( "causes the music sequencer to go to the head of the song. " );
				setLongDescription( ""
									+ "This procedure usually causes the music sequencer to call the main-procedure. "
									+ "See (help set-main). "
									+ THROWS_AN_ERROR_IF_NOT_OPEN );
			}}, 
			"rewind" );
		
		DProcedureN simul = new DProcedureN( "simultaneous" ) {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				synchronized ( getMetroLock() ) {
					try {
						enterTrackChangeBlock();
						for ( int i=0; i<args.length; i++ ) {
							Object arg = args[i];
							if ( arg instanceof Procedure ) {
								((Procedure)arg).apply0();
							} else {
								logWarn( "The value in args[" + i + "] was not a procedure. Ignored. "  );
							}
						}
					} finally {
						leaveTrackChangeBlock();
						notifyTrackChange();
					}
				}
				return Invokable.NO_RESULT;
			}
		};
		SchemeUtils.defineVar( scheme, simul , "simultaneous", "simul" );
		SchemeUtils.setDocumentInitializer( scheme,
			new DescriptiveInitializerA(){{
				setParameterDescription( "[procedure ...]" );
				setReturnValueDescription( "::void" );
				setShortDescription( "executes passed the procedures \"simultaneously\". " );
				setLongDescription( ""
									+ "This procedure is designed to add multiple tracks to the sequencer and and let the tracks start "
									+ "to play simultaneously. While the interpreter is executing this procedure, the thread "
									+ "that is processing tracks in order to generate the music data is blocked. "
									+ "Therefore, it is guaranteed that the sequencer starts to process the added tracks simultaneously. "
									+ "" 
									+ THROWS_AN_ERROR_IF_NOT_OPEN );
			}}, 
			"simultaneous" );
		
		/////////////////////////////////////////////////////////////////

		DProcedureN getTrack = new DProcedureN( "get-track" ) {
			Invokable searchTrackFilter( Invokable i ) {
				return new Invokable() {
					@Override
					public Object invoke(Object... args) {
						if ( 0 < args.length ) {
							args[1] = filterArg( args[1] );
						}
						return i.invoke( args );
					}
					Object filterArg(Object arg1) {
						if ( arg1 == null ) {
							return EmptyList.emptyList;
						} else if ( arg1 instanceof LList ) {
							return arg1;
						} else if ( arg1 instanceof Collection ) {
							if (((Collection)arg1).isEmpty()) {
								return EmptyList.emptyList;
							} else {
								return Pair.makeList( Arrays.asList(((Collection)arg1).toArray()));
							}
						} else {
							return arg1;
						}
					}
				};
			}
			
			@Override
			public Object applyN(Object[] args) throws Throwable {
				ArrayList<MetroTrack> t = new ArrayList<>();
				for ( int i=0; i<args.length; i++ ) {
					t.addAll( 
						searchTrack(
							searchTrackFilter(
								createInvokable( 
									readParamTrackSearcher( args[i] ))))) ;
				}
				return Pair.makeList( t );
			}
		};
		SchemeUtils.defineVar( scheme, getTrack, "get-track"
											   , "gett" );
		
		SchemeUtils.setDocumentInitializer( scheme,
			new DescriptiveInitializerA(){{
				setParameterDescription( "[track-spec ...]" );
				setReturnValueDescription( "::void" );
				setShortDescription( "retrieves multiple tracks which are specified as track-spec arguments and returns them as a list." );
				setLongDescription( ""
									+ "The track-spec is a specification of a track to retrieve. "
									+ "Any of following values are valid as a track-spec : "
									+ "procedure, symbol and string. \n\n"
									+ "track-spec=procedure: The system enumerates all tracks in the current sequencer, "
									+ "and call the given procedure for each track. The procedure should have two parameters : "
									+ "(lambda ( name tags ) ... ). If a track with the name and the tags is not what you want, "
									+ "the procedure should return #f; otherwise the track is added to the result. \n\n"
									+ "track-spec=symbol/string: the value is compared with the name value "
									+ "of each track, and the track is added to the result when it equals to the value. "
									+ "It uses the equals() method of java.lang.Object class to compare the values. \n\n"
									+ "" 
									+ THROWS_AN_ERROR_IF_NOT_OPEN );
			}}, 
			"get-track" );
		/////////////////////////////////////////////////////////////////

		DProcedureN newTrack = new DProcedureN( "new-track" ) {
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
						procedure = readParamProcedure(args[0]);
						break;
					case 2 : {
						List<Object> lst = readParamTrackName( args[0] );
						name = lst.remove(0);
						tags = lst;
						procedure = readParamProcedure(args[1]);
						break;
					}	
					default :
						throw new IllegalArgumentException();
				}
				return createTrack( name, tags, procedure );
			}
		};
		SchemeUtils.defineVar( scheme, newTrack , "new-track"
											    , "newt" );
		
		SchemeUtils.setDocumentInitializer( scheme,
			new DescriptiveInitializerA(){{
				setParameterDescription( "[track-spec ...]" );
				setReturnValueDescription( "::void" );
				setShortDescription( "retrieves multiple tracks which are specified as track-spec arguments and returns them as a list." );
				setLongDescription( ""
									+ "The track-spec is a specification of a track to retrieve. "
									+ "Any of following values are valid as a track-spec : "
									+ "procedure, symbol and string. \n\n"
									+ "track-spec=procedure: The system enumerates all tracks in the current sequencer, "
									+ "and call the given procedure for each track. The procedure should have two parameters : "
									+ "(lambda ( name tags ) ... ). If a track with the name and the tags is not what you want, "
									+ "the procedure should return #f; otherwise the track is added to the result. \n\n"
									+ "track-spec=symbol/string: the value is compared with the name value "
									+ "of each track, and the track is added to the result when it equals to the value. "
									+ "It uses the equals() method of java.lang.Object class to compare the values. \n\n"
									+ "" 
									+ THROWS_AN_ERROR_IF_NOT_OPEN );
			}}, 
			"get-track" );
		/////////////////////////////////////////////////////////////////
		
		abstract class TrackManagementProcedure extends DProcedureN  {
			TrackManagementProcedure( String name ) {
				super(name);
			}
			abstract void procTrack( List<MetroTrack> trackList, SyncType syncType, MetroTrack syncTrack, double syncOffset );
			
			@Override
			public Object applyN(Object[] args) throws Throwable {
				List<MetroTrack> trackList;
				SyncType syncType;
				List<MetroTrack> syncTrackList;
				double syncOffset;
				switch ( args.length ) {
					case 0 :
						throw new IllegalArgumentException();
					case 1 :
						trackList     = readParamTrack( args[0] );
						syncType      = SyncType.IMMEDIATE;
						syncTrackList = Collections.EMPTY_LIST;
						syncOffset    = 0.0d;
						break;
					case 2 :
						trackList     = readParamTrack( args[0] );
						syncType      = readParamSyncType( args[1] );
						syncTrackList = Collections.EMPTY_LIST;
						syncOffset    = 0.0d;
						break;
					case 3 :
						trackList     = readParamTrack( args[0] );
						syncType      = readParamSyncType( args[1] );
						syncTrackList = readParamTrack( args[2] );
						syncOffset    = 0.0d;
						break;
					case 4 :
						trackList     = readParamTrack( args[0] );
						syncType      = readParamSyncType( args[1] );
						syncTrackList = readParamTrack( args[2] );
						syncOffset    = readParamSyncOffset( args[3] );
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
				
				return Invokable.NO_RESULT;
			}
		}

		/////////////////////////////////////////////////////////////////

		DProcedureN putTrack = new TrackManagementProcedure( "put-track" ) {
			@Override
			void procTrack( List<MetroTrack> trackList, SyncType syncType, MetroTrack syncTrack, double syncOffset ) {
				putTrack(trackList, syncType, syncTrack, syncOffset);
			}
		};
		SchemeUtils.defineVar( scheme, putTrack , "put-track"
												, "putt" );

		/////////////////////////////////////////////////////////////////

		DProcedureN removeTrack = new TrackManagementProcedure( "remove-track" ) {
			@Override
			void procTrack( List<MetroTrack> trackList, SyncType syncType, MetroTrack syncTrack, double syncOffset ) {
				removeTrack(trackList, syncType, syncTrack, syncOffset);
			}
		};
		SchemeUtils.defineVar( scheme, removeTrack , "remove-track"
												   , "remt" );
		

		Procedure notifyTrackChange = new DProcedure0( "notify-track-change" ) {
			@Override
			public Object apply0() throws Throwable {
				notifyTrackChange();
				return Invokable.NO_RESULT;
			}
		};
		SchemeUtils.defineVar( scheme, notifyTrackChange , "notify-track-change", 
														   "nott" );

		SchemeUtils.defineVar( scheme, new DProcedure0("ls") {
			@Override
			public Object apply0() throws Throwable {
				List<MetroTrack> tempAllTracks = replicateAllTracks(); 
				ArrayList<Object> list = new ArrayList<>( tempAllTracks.size() );
				for ( MetroTrack track :  tempAllTracks ) {
					list.add( track );
				}
				Collections.reverse(list);
				return Pair.makeList(list);

			}
		} , "lst");

		DProcedure0 clr = new DProcedure0("clear-tracks") {
			@Override
			public Object apply0() throws Throwable {
				clearTracks();
				return Invokable.NO_RESULT;
			}
		};
		SchemeUtils.defineVar( scheme, clr , "clet"
										   , "clear-tracks" );
		
		SchemeUtils.defineVar( scheme, new DProcedureN("print-stack-trace") {
			@Override
			public Object apply1(Object arg) throws Throwable {
				((Throwable)arg).printStackTrace();
				return arg;
			}
		} , "print-stack-trace");
		SchemeUtils.defineVar( scheme, new DProcedureN("display-warn") {
			@Override
			public Object apply1(Object arg) throws Throwable {
				System.err.print( arg );
				return Values.empty;
			}
		} , "display-warn");
		SchemeUtils.defineVar( scheme, new DProcedure0("newline-warn") {
			@Override
			public Object apply0() throws Throwable {
				System.err.println();
				return Values.empty;
			}
		} , "newline-warn");

		SchemeUtils.defineVar( scheme, new DProcedureN("list-seq") {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				//					logInfo("list-seq");
				List<MetroTrack> tempAllTracks = replicateAllTracks();
				synchronized ( getMetroLock() ) {
					ArrayList<LList> list = new ArrayList<>( tempAllTracks.size() );
					for ( MetroTrack track :  tempAllTracks ) {
						SchemeSequence sequence = (SchemeSequence)track.getSequence();
						list.add( Pair.make( sequence.getTrackName(), sequence.asociationList ));
					}
					return LList.makeList(list);
				}
			}
		}, "list-seq" );

		SchemeUtils.defineVar( scheme, new DProcedureN("typeof") {
			public Object applyN(Object[] args) throws Throwable {
				if ( 0 < args.length  ) {
					return args[0].getClass().getName();
				} else {
					return Invokable.NO_RESULT;
				}
			}
		} , "typeof");

		SchemeUtils.defineVar( scheme, new DProcedure3("mktimer") {
			@Override
			public Object apply3(Object arg0, Object arg1,Object arg2 ) throws Throwable {
				Runnable runnable = createTimer( Pulsar.this, 
					SchemeUtils.toInteger( arg0 ), 
					SchemeUtils.toInteger( arg1 ), 
					Pulsar.this.createInvokable2( (Procedure)arg2 ) );

				return new DProcedure0() {
					public Object apply0() throws Throwable {
						runnable.run();
						return Values.noArgs;
					};
				};
			}
		} , "mktimer");

		SchemeUtils.defineVar( scheme, new DProcedureN("rnd") {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				switch ( args.length ) {
					case 0 :
						return DFloNum.valueOf( random.nextDouble() );
					case 1 : {
						double range = SchemeUtils.toDouble( args[0] );
						return DFloNum.valueOf( random.nextDouble() * range );
					}
					default :
					{
						double rangeMin = SchemeUtils.toDouble( args[0] );
						double rangeMax = SchemeUtils.toDouble( args[1] );
						double range    = rangeMax - rangeMin;

						return DFloNum.valueOf( random.nextDouble() * range + rangeMin );
					}
				}
			}
		} , "rnd");
		
		SchemeUtils.defineVar( scheme, new DProcedureN("luck") {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				double probability = args.length == 0 ? 0.5 : SchemeUtils.toDouble( args[0] );
				if ( probability < 0 ) return false;
				if ( 1.0<=probability  ) return true;
				return random.nextBoolean( probability );
			}
			{
				setParameterDescription( "[numeric]" );
				setReturnValueDescription( "::boolean" );
				setShortDescription( "is a procedure that returns a random bool value. " );
				setLongDescription( "The first argument is the value of probability "
						+ "where the larger value causes the more probability of returning #t. "
						+ "When the specified value is equals or less than zero, the returning value is always #f. "
						+ "When the specified value is equals or larger than one the returning value is always #t. "
						+ "The only parameter can be omitted and in that case the default value one is applied. " );
			}

		} , "luck");

		SchemeUtils.defineVar( scheme, new DProcedureN("help!") {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				return "Calm down!";
			}
			{
				setParameterDescription( "" );
				setReturnValueDescription( "::string" );
				setShortDescription(  "is a procedure to execute when the user needs something that possibly calms you down." );
				setLongDescription( 
					"When this procedure is called, this procedure will return a message which "
					+ "tries to calm the user down. Any argument specified to this procedure will be silently ignored."
					+ "This procedure is intended to be defined as a joke and has by no means effect to the current system state "
					+ "nor any other related elements." );
			}
		} , "help!");

		final class DProcedureHelp extends DProcedureN {
			final int index;
			final Procedure reverse = (Procedure)gnu.kawa.slib.srfi1.reverse.get();
			final Procedure map = (Procedure)gnu.kawa.slib.srfi1.map.get();
			
			private DProcedureHelp( String name, int index ) {
				super(name);
				this.index = index;
			}
			
			public Object apply0() throws Throwable {
				Procedure1 proc1 = new Procedure1() {
					@Override
					public Object apply1(Object arg1) throws Throwable {
						Pair pair = (Pair)arg1;
						if ( index < pair.length() ) {
							return pair.get(index);
						} else if ( 1 < pair.length() ) {
							return pair.get(1);
						} else if ( 0 < pair.length() ) {
							return Symbol.valueOf(((Procedure)pair.get(0)).getName());
						} else {
							return "";
						}
					}
				};
				return map.apply2( proc1, 
					reverse.apply1( 
						SchemeUtils.getDocumentList()));
			}; 
			
			String MSG_NO_DOCUMENTATION = "No documentation is available.";
			public Object apply1(Object arg1) throws Throwable {

				String message ="" ;
				
				if ( arg1 instanceof DescriptiveProcedure ) {
					DescriptiveProcedure proc = (DescriptiveProcedure)arg1;
					List<String> names = SchemeUtils.symbolListToStringList( proc.getNameList());
					
					message += "=== THE MANUAL OF PULSAR LISP SCHEME MUSIC SEQUENCER ===\n\n";
					message += "NAME: ";
					message += names.isEmpty() ? "PULSAR" :  names.get(0).toUpperCase() ;
					message += "\n\n";
					{
						String syn = proc.getParameterDescription();
						String rv = proc.getReturnValueDescription();
						message = message +
								"SYNOPSIS: (" +
								String.join( "|", 
									names) +
								(syn.equals("") ? "" : " ") +
								syn +
								")" + rv;
						
						message += "\n\n";
					}

					String msg1;
					{
						msg1 = proc.getShortDescription();
						if ( "".equals( msg1 ) ) {
						} else {
							List<Symbol> l = proc.getNameList();
							if ( l != null && ! l.isEmpty() ) {
								msg1 = "||" + SchemeUtils.symbolToString( l.get(0) ) + "||" + " " + msg1;
							} else {
								msg1 = "This " + msg1;
							}
						}
					}

					String msg2;
					{
						msg2 = proc.getLongDescription();
					}
					message += SchemeUtils.wrapMultiLine(
						"DESCRIPTION: " + 
						(msg1 + " " + msg2).trim() , 80 ).trim();
					
					message += "\n\n";
				} else {
					message = MSG_NO_DOCUMENTATION;
				}
				return SchemeUtils.toSchemeString(message );
			};


			@Override
			public Object applyN(Object[] args) throws Throwable {
				if ( args.length == 0 )
					return apply0();
				else if ( args.length == 1 )
					return apply1(args[0]);
				else
				    throw new WrongArguments( this, args.length );

			}
			{
				setParameterDescription( "[procedure]" );
				setReturnValueDescription( "::string|list" );
				setShortDescription( "is a procedure to show the description of a specified procedure." ); 
				setLongDescription( 
						"When a procedure reference is passed, it returns a string value that contains "
						+ "the description of the the procedure. \n\n"
						+ "If no procedure is specified, it returns a list that contains all procedures which "
						+ "description is available." );
			}
		}
		SchemeUtils.defineVar( scheme, new DProcedureHelp( "help", 1 ) , "help", "he" );

		{
			SchemeUtils.execScheme( Pulsar.class, scheme, "lib/init.scm"  );
			SchemeUtils.execScheme( Pulsar.class, scheme, "lib/basic-notes.scm"  );
			SchemeUtils.execScheme( Pulsar.class, scheme, "lib/music.scm"  );
			SchemeUtils.execScheme( Pulsar.class, scheme, "lib/xnoop.scm" );
		}
	}
}
	
