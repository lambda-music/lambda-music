package pulsar;

import static pulsar.PulsarLib.PulsarLibImplementation.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import gnu.expr.SourceName;
import gnu.mapping.Procedure;
import lamu.lib.helps.LamuDocument;
import lamu.lib.kawautils.SchemeValues;
import lamu.lib.kawautils.procedures.MultipleNamedProcedureN;
import metro.MetroPort;
import metro.MetroPortSelector;
import metro.MetroTrackManipulator;
import metro.MetroTrackSelector;
import metro.MetroTrackSynchronizer;
import pulsar.PulsarLib.PulsarLibImplementation.PulsarProceduralDescriptiveDoc;

public final class PulsarLib_Static {
	private PulsarLib_Static(){
	}
	
	@SourceName(name = "newt")
	public static final Procedure newTrackProc = new NewTrackProc( new String[] { "new-track", "newt" });
	public static final class NewTrackProc extends MultipleNamedProcedureN {
		public NewTrackProc(String[] names) {
			super(names);
		}
		@Override
		public Object applyN(Object[] args) throws Throwable {
			return readParamNewTrack(args);
		}
	}

	@SourceName(name = "newt-doc")
	public static final NewTrackDoc newTrackDoc = new NewTrackDoc();
	public static final class NewTrackDoc extends PulsarProceduralDescriptiveDoc {
		{
			setCategory( Pulsar.DOCS_ID );
			setNames( "newt" );
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
		}
	}

	/**
	 * This class was created but not tested. (Sat, 27 Jun 2020 11:16:51 +0900)
	 */
	static final class PulsarRecordingPortSelector implements MetroPortSelector {
		private final Object inputPortName;
		private final Object outputPortName;

		private PulsarRecordingPortSelector(Object inputPortName, Object outputPortName) {
			this.inputPortName = inputPortName;
			this.outputPortName = outputPortName;
		}

		@Override
		public void selectPort(
				List<MetroPort> inputPorts, 
				List<MetroPort> outputPorts, 
				List<MetroPort> selectedInputPorts,
				List<MetroPort> selectedOutputPorts )
		{
			{
				List<MetroPort> ports = readParamPort( inputPortName, inputPorts );
				if ( ports.size() == 0 )
					throw new IllegalArgumentException("could not find input port " + inputPortName );
				selectedInputPorts.addAll( ports ); 
			}
			{
				List<MetroPort> ports = readParamPort( outputPortName, outputPorts );
				if ( ports.size() == 0 )
					throw new IllegalArgumentException("could not find output port " + outputPortName );
				selectedOutputPorts.addAll( ports ); 
			}
		}
	}

	@SourceName(name = "rect")
	public static final Procedure newRecordingTrackProc = new NewRecordTrackProc( new String[] { "new-recording-track", "rect" });
	public static final class NewRecordTrackProc extends MultipleNamedProcedureN {
		public NewRecordTrackProc(String[] names) {
			super(names);
		}

		@Override
		public Object applyN(Object[] args) throws Throwable {
			double recordLength;
			boolean looper;

			MetroPortSelector portSelector;
			switch ( args.length  ){
			case 0 :
			case 1 : 
			case 2 : 
				throw new IllegalArgumentException();
			case 3 : 
			case 4 : 
			case 5 : 
			{
				Object inputPortName = args[0];
				Object outputPortName = args[1];

				portSelector = new PulsarRecordingPortSelector(inputPortName, outputPortName);

				if ( 3< args.length ) {
					recordLength = SchemeValues.toDouble( args[2] );
				} else {
					recordLength = -1;
				}
				if ( 4< args.length ) {
					looper = SchemeValues.toBoolean( args[3] );
				} else {
					looper = true;
				}
				break;
			}

			default :
				throw new IllegalArgumentException();
			}
			return PulsarCommon.createRecordingSequence( portSelector, recordLength, looper );
		}
	}

	public static final NewRecordingTrackDoc newRecordingTrackDoc = new NewRecordingTrackDoc();
	public static final class NewRecordingTrackDoc extends PulsarProceduralDescriptiveDoc {
		{
			setCategory( Pulsar.DOCS_ID );
			setNames( "new-recording-track" , "rect" );
			setParameterDescription( "[procedure/(list notation)]..." );
			addParameter( 0, "notations", "procedure/(list notation)", null, true, "The contents of the track. " );
			setReturnValueDescription( "::MetroTrack" );
			setShortDescription( "<name/> creates a new track." );
			setLongDescription( ""
					+ "" 
					+ THROWS_AN_ERROR_IF_NOT_OPEN );
		}
	}

	@SourceName(name = "synct")
	public static final Procedure syncTrackProc = new SyncTrackProc( new String[] {  "synchronize-track", "synct" });
	public static final class SyncTrackProc extends MultipleNamedProcedureN {
		public SyncTrackProc(String[] names) {
			super(names);
		}
		@Override
		public Object applyN(Object[] args) throws Throwable {
			return readParamSynct(args);
		}
	}

	@SourceName(name = "selt")
	public static final Procedure selectTrackProc = new SelectTrackProc( new String[] { "select-track", "selt" });
	public static final class SelectTrackProc extends MultipleNamedProcedureN {
		public SelectTrackProc(String[] names) {
			super(names);
		}
		@Override
		public Object applyN(Object[] args) throws Throwable {
			return readParamSelt(args);
		}
	}


	@SourceName(name = "mant")
	public static final Procedure manipulateTrackProc = new ManipulateTrackProc( new String[] { "manipulate-track", "mant" });
	public static final class ManipulateTrackProc extends MultipleNamedProcedureN {
		public ManipulateTrackProc(String[] names) {
			super(names);
		}
		@Override
		public Object applyN(Object[] args) throws Throwable {
			switch ( args.length ) {
			case 0:
				return readParamMant( "idle" );
			case 1:
				return readParamMant( SchemeValues.anyToString( args[0] ) );
			default :
				return readParamMant( 
						SchemeValues.anyToString( args[0] ),
						Arrays.copyOfRange(args, 1, args.length ) );
			}
		}
	}

	@SourceName(name = "remt")
	public static final Procedure removeTrackProc = new RemoveTrackProc(new String[] { "remove-track", "remt" });
	public static final class RemoveTrackProc extends MultipleNamedProcedureN {
		public RemoveTrackProc(String[] names) {
			super(names);
		}

		// Reuse the objects for passing parameters to reduce the garbage-collector load.  
		final Map<String, Object> namedArgs = new HashMap<>();
		final List<Object> plainArgs = new ArrayList<>();
		final Object[] trackManipulators = new Object[1]; 

		@Override
		public synchronized Object applyN(Object[] args) throws Throwable {
			namedArgs.clear();
			plainArgs.clear();
			SchemeValues.parseArguments(args, namedArgs, plainArgs);

			MetroTrackSynchronizer trackSynchronizer = 
					(MetroTrackSynchronizer) namedArgs.get("stop");

			MetroTrackSelector trackSelector = readParamSelt(plainArgs.toArray(new Object[plainArgs.size()]));
			MetroTrackManipulator trackManipulator = readParamMant( 
					"remt", 
					trackSelector, 
					trackSynchronizer);

			return trackManipulator;
		}
	}

	public static final LamuDocument removeTrackDoc = trackManagementTemplateDoc.processArguments( 
			"removes",
			""
					+ "The sequencer remove the specified track. Eventually the track stops playing. "
					+ "And it gives the user some controls on "
					+ "how it stops playing the track. "    
			).setNames( "remove-track", "remt" );


	@SourceName(name = "putt")
	public static final Procedure putTrackProc = new PutTrackProc(new String[] { "put-track", "putt" });
	public static final class PutTrackProc extends MultipleNamedProcedureN {
		public PutTrackProc(String[] names) {
			super(names);
		}

		// Reuse the objects for passing parameters to reduce the garbage-collector load.  
		final Map<String, Object> namedArgs = new HashMap<>();
		final List<Object> plainArgs = new ArrayList<>();
		final Object[] trackManipulators = new Object[1]; 

		@Override
		public synchronized Object applyN(Object[] args) throws Throwable {
			namedArgs.clear();
			plainArgs.clear();
			SchemeValues.parseArguments(args, namedArgs, plainArgs);

			MetroTrackSynchronizer trackSynchronizer = 
					(MetroTrackSynchronizer) namedArgs.get("start");

			plainArgs.add(0, "newt");
			MetroTrackSelector trackSelectors = readParamSelt(plainArgs.toArray(new Object[plainArgs.size()]));
			MetroTrackManipulator trackManipulator = readParamMant( 
					"putt", 
					trackSelectors, 
					trackSynchronizer);

			return trackManipulator;
		}
	}
	public static final LamuDocument putTrackDoc = trackManagementTemplateDoc.processArguments( 
			"put",
			""
					+ "The sequencer starts to play the added track and it gives the user some controls on "
					+ "how it starts playing the track."
			).setNames( "put-track", "putt" );


	@SourceName(name = "rept")
	public static final Procedure replaceTrackProc = new ReplaceTrackProc(new String[] { "replace-track", "rept" });
	public static final class ReplaceTrackProc extends MultipleNamedProcedureN {
		public ReplaceTrackProc(String[] names) {
			super(names);
		}

		// Reuse the objects for passing parameters to reduce the garbage-collector load.  
		final Map<String, Object> namedArgs = new HashMap<>();
		final List<Object> plainArgs = new ArrayList<>();
		final Object[] trackManipulators = new Object[1]; 

		@Override
		public synchronized Object applyN(Object[] args) throws Throwable {
			namedArgs.clear();
			plainArgs.clear();
			SchemeValues.parseArguments(args, namedArgs, plainArgs);

			MetroTrackSynchronizer startSynchronizer = 
					(MetroTrackSynchronizer) namedArgs.get("start");
			MetroTrackSynchronizer stopSynchronizer = 
					(MetroTrackSynchronizer) namedArgs.get("stop");

			plainArgs.add(0, "newt");
			MetroTrackSelector trackSelectors = readParamSelt(plainArgs.toArray(new Object[plainArgs.size()]));
			MetroTrackManipulator trackManipulator = readParamMant( 
					"rept", 
					trackSelectors,
					startSynchronizer,
					stopSynchronizer );

			return trackManipulator;
		}
	}


	public static final LamuDocument replaceTrackDoc = trackManagementTemplateDoc.processArguments( 
			"replace",
			""
					+ "The sequencer starts to play the added track and it gives the user some controls on "
					+ "how it starts playing the track."
			).setNames( "put-track", "putt" );

	
	@SourceName( name = "gett" )
    public static final Procedure getTrackProc = new GetTrackProc(new String[] { "get-track", "gett" });
    public static final class GetTrackProc extends MultipleNamedProcedureN {
        public GetTrackProc(String[] names) {
            super(names);
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            // ADDED (Thu, 25 Jun 2020 23:56:05 +0900) >>>
            return readParamGett(args);
            // ADDED (Thu, 25 Jun 2020 23:56:05 +0900) <<<
        }
    }

    public static final GetTrackDoc getTrackDoc = new GetTrackDoc();
    public static final class GetTrackDoc extends PulsarProceduralDescriptiveDoc {
        {
            setCategory( Pulsar.DOCS_ID );
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
        }
    }

}
