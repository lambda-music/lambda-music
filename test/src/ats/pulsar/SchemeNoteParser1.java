package ats.pulsar;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.logging.Level;
import java.util.logging.Logger;

import ats.metro.Metro;
import ats.metro.MetroMidiMessageGen;
import ats.metro.MetroEventBuffer;
import ats.metro.MetroTrack.SyncType;
import ats.pulsar.lib.SchemeUtils;
import gnu.lists.AbstractSequence;
import gnu.lists.Pair;
import gnu.mapping.Environment;
import gnu.mapping.Procedure;
import kawa.standard.Scheme;

/**
 * http://nickfever.com/music/midi-cc-list
 * 
 * @author ats
 *
 */
@SuppressWarnings("unused")
public class SchemeNoteParser1 {
	static final Logger LOGGER = Logger.getLogger(SchemeNoteParser1.class.getName());
	static final String ID_TYPE      = "type";
	static final String ID_ENABLED   = "enab";
	static final String ID_CHANNEL   = "chan";
	static final String ID_PORT_NO   = "port";
	static final String ID_PROCEDURE = "proc";
	static final String ID_ID        = "id";
	static final String ID_LENGTH    = "len";
	static final String ID_VELOCITY  = "velo";
	static final String ID_NOTE      = "note";
	static final String ID_OFFSET    = "pos";
	static final String ID_KEY       = "key";
	static final String ID_MIN       = "min";
	static final String ID_MAX       = "max";
	static final String ID_VALUE     = "val";

	public static boolean parse( Metro metro, Scheme scheme, AbstractSequence<Object> inputList, MetroEventBuffer outputBuffer, boolean result ) {
		// boolean result = true;
		if ( inputList != null ) {
			for ( Iterator<Object> i = inputList.iterator(); i.hasNext(); ) {
 				Object obj = i.next();
				if ( obj instanceof Pair ) {
					Pair record = (Pair)obj;
					result = parseNote( metro, scheme, outputBuffer, result, record );
				} else if ( obj instanceof Boolean ) {
					continue;
				} else {
					SchemeSequence.LOGGER.log( Level.WARNING, "Unsupported object type was found. We ignored it." + obj );
				}
					
			} // end of the loop
		}
		// buf.setLength( this.bars );
		// buf.noteShot(0, 1, 0, 73, 100 );
		return result;
	}

	static final <V> V getValue( Map<String,Object> map, String key, V defaultValue, Function<Object,V> converter ) {
		if ( map.containsKey(key) )
			return converter.apply( map.get(key) );
		else
			return defaultValue;
	}

	static abstract class SchemeEventParser {
		String id;
		String name;
		String shortDescription;
		String description;
		abstract boolean parseEvent( Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String,Object> map, boolean result );
	}
	
	static HashMap<String,SchemeEventParser> parserMapping = new HashMap<String,SchemeEventParser>();
	static void putParser( SchemeEventParser parser ) {
		String id = parser.id;
		if ( parserMapping.containsKey(id))
			throw new RuntimeException( "internal error : id (" + id + ") is already registered." );
		
		parserMapping.put( id, parser );
	}
	static SchemeEventParser getParser( String id ) {
		return parserMapping.get(id);
	}
	
	static abstract class SchemeMidiControlChangeEventParser extends SchemeEventParser {
		int controlNumber;
	}

	
	static final class VoidEventParser extends SchemeEventParser {
		{
			this.id = "void";
			this.name = "void";
		}
		@Override
		boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
			return result;
		}
	}
	static abstract class NoteEventParser extends SchemeEventParser {
		@Override
		boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
			boolean enabled      = map.containsKey( ID_ENABLED     ) ? SchemeUtils.toBoolean(       map.get(ID_ENABLED      ) ) : true;
			if ( ! enabled )
				return result;

			int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
			int channel      = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
			double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
			int note         = map.containsKey( ID_NOTE     ) ? SchemeUtils.toInteger(      map.get(ID_NOTE      ) ) : 63;  
			double velocity  = map.containsKey( ID_VELOCITY ) ? SchemeUtils.toDouble(       map.get(ID_VELOCITY  ) ) : 63;
			double length    = map.containsKey( ID_LENGTH   ) ? SchemeUtils.toDouble(       map.get(ID_LENGTH    ) ) : -1d;
			
			proc( scheme, outputBuffer, offset, port, channel, note, velocity, length );
			
			return result;
		}

		abstract void proc(Scheme scheme, MetroEventBuffer outputBuffer, 
				double offset, int port, int channel, int note, double velocity, double duration );
	}
	
	static final class NoteHitEventParser extends NoteEventParser {
		{
			this.id = "note";
			this.name = "note hit";
		}
		@Override
		void proc(Scheme scheme, MetroEventBuffer outputBuffer, 
				double offset, int port, int channel, int note,double velocity, double duration ) 
		{
//			duration = 0.5;
			if ( duration < 0 )
				duration = 0.0025d;

//			System.out.println( channel );

//			outputBuffer.noteHit( offset, port, channel, note, velocity, length );
			outputBuffer.midiEvent(offset             , port, MetroMidiMessageGen.noteOn (channel, note, velocity ) );
			outputBuffer.midiEvent(offset + duration  , port, MetroMidiMessageGen.noteOff(channel, note, velocity ) );
		}
	}
	static final class NoteOnEventParser extends NoteEventParser {
		{
			this.id = "non";
			this.name = "note on";
		}
		@Override
		void proc(Scheme scheme, MetroEventBuffer outputBuffer, 
				double offset, int port, int channel, int note,double velocity, double duration ) 
		{
//			outputBuffer.noteOn( offset, port, channel, note, velocity );
			outputBuffer.midiEvent(offset, port, MetroMidiMessageGen.noteOn (channel, note, velocity ) );
		}
	}
	static final class NoteOffEventParser extends NoteEventParser {
		{
			this.id = "noff";
			this.name = "note off";
		}
		@Override
		void proc(Scheme scheme, MetroEventBuffer outputBuffer, 
				double offset, int port, int channel, int note,double velocity, double duration ) 
		{
//			outputBuffer.noteOff( offset, port, channel, note, velocity );
			outputBuffer.midiEvent(offset, port, MetroMidiMessageGen.noteOff(channel, note, velocity ) );
		}
	}

	static final class BarEventParser extends SchemeEventParser {
		{
			this.id = "len";
			this.name = "bar length";
		}
		@Override
		boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
			double value    = map.containsKey( ID_VALUE ) ? SchemeUtils.toDouble( map.get( ID_VALUE ) ) : -1.0d;
			if ( value < 0 ) {
				LOGGER.log( Level.WARNING, "a len note was found but 'val was missing. This probably a bug." );
				value = 0.0d;
			}

			outputBuffer.setLength( value );
			return result;
		}
	}
	static final class ExecEventParser extends SchemeEventParser {
		{
			this.id = "exec";
			this.name = "execute procedure";
		}
		@Override
		boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
			double offset        = map.containsKey( ID_OFFSET   )  ? SchemeUtils.toDouble(     map.get( ID_OFFSET    ) ) : 0.0d;
			Procedure procedure0 = map.containsKey( ID_PROCEDURE ) ?                (Procedure)map.get( ID_PROCEDURE )   : null;
			// See the note ... XXX_SYNC_01
			outputBuffer.exec( offset, 
					new RunnableSchemeProcedure( 
							new InvocableSchemeProcedure( scheme /* XXX_SYNC_01 */, Environment.getCurrent(), procedure0) ) );
			return result;
		}
	}
//	private static final Procedure SCHEME_NOP = new ProcedureN() {
//	};
	static final class PutEventParser extends SchemeEventParser {
		{
			this.id = "add";
			this.name = "add a new sequence";
		}
		@Override
		boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
			List<String> el = Collections.emptyList();

			double offset            = getValue( map, ID_OFFSET, 0.0d, (v)-> SchemeUtils.toDouble( v )   );
			String id                = getValue( map, "id",   null, (v)-> SchemeUtils.anyToString(    SchemeUtils.schemeNullCheck( v ) ) );
			Collection<String> tags  = getValue( map, "tags", el,   (v)-> SchemeUtils.symbolListToStringList((Pair)v ) );
			SyncType syncType        = getValue( map, "syty", SyncType.SERIAL, (v)-> SyncType.valueOf( SchemeUtils.symbolToString( SchemeUtils.schemeNullCheck( v ) ).toUpperCase() ) );
			String syncSequenceId    = getValue( map, "syid", "seq-base", (v)-> SchemeUtils.anyToString(    SchemeUtils.schemeNullCheck( v ) ) );
			double syncOffset        = getValue( map, "syof", 0.0d, (v)-> SchemeUtils.toDouble( v )   );
			Procedure procedure      = getValue( map, ID_PROCEDURE, null, (v)->(Procedure)v );

			if ( id == null )
				id = createDefaultId(scheme);

			if ( procedure != null ) {
				Pulsar pulsar = (Pulsar)metro;
				
				String id2= id;
				// See the note ... XXX_SYNC_01
				outputBuffer.exec( offset, new Runnable() {
					@Override
					public void run() {
						SchemeSequence sequence = new SchemeSequence( scheme,
								new InvocableSchemeProcedure( scheme, Environment.getCurrent(), procedure ) );

						pulsar.putSequence( id2, tags, sequence, syncType, syncSequenceId, syncOffset  );
					}
				} );
			}
			return result;
		}
		public String createDefaultId(Scheme scheme) {
			String id;
			try {
				id = SchemeUtils.symbolToString( scheme.eval( "(make-default-id)" ) );
			} catch (Throwable e) {
				LOGGER.log(Level.WARNING, "put: Ignored an error. Default value was applied to the id.: ", e);
				id = "seq-default";
			}
			return id;
		}
	}
	static final class RemoveEventParser extends SchemeEventParser {
		{
			this.id = "kill";
			this.name = "Remove the specified sequence";
		}
		@Override
		boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
			List<String> el = Collections.emptyList();

			double offset            = getValue( map, ID_OFFSET, 0.0d, (v)-> SchemeUtils.toDouble( v )   );
			String id                = getValue( map, "id",   null, (v)-> SchemeUtils.anyToString(    SchemeUtils.schemeNullCheck( v ) ) );
			Collection<String> tags  = getValue( map, "tags", el,   (v)-> SchemeUtils.symbolListToStringList((Pair)v ) );
			
			Pulsar pulsar = (Pulsar)metro;


			if ( id != null ) {
				outputBuffer.exec( offset, new Runnable() {
					@Override
					public void run() {
						pulsar.removeTrack( id );
					}
				} );
			}

			if ( tags != null ) {
				outputBuffer.exec( offset, new Runnable() {
					@Override
					public void run() {
						pulsar.removeTrackAll( pulsar.getTrackByTags(tags) );
					}
				} );
			}
			return result;
		}
	}
	
	static final class ListEventParser extends SchemeEventParser {
		{
			this.id = "list";
			this.name = "list";
		}
		@Override
		boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
			AbstractSequence<Object> value = map.containsKey( ID_VALUE ) ? (AbstractSequence<Object> )map.get( ID_VALUE ) : null;
			if ( value != null ) {
				// *** a recursive calling ***
				result = SchemeNoteParser1.parse(metro, scheme , value, outputBuffer, result );
			} else {
				LOGGER.log(Level.WARNING, "Found an empty list. This might be a possible cause of problems" );
			}
			return result;
		}
	}
	static final class EndEventParser extends SchemeEventParser {
		{
			this.id = "end";
			this.name = "end the sequence";
		}
		@Override
		boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
			return false;
		}
	}


	static {
		putParser( new VoidEventParser());
		putParser( new NoteHitEventParser());
		putParser( new NoteOnEventParser());
		putParser( new NoteOffEventParser());
		putParser( new BarEventParser());
		putParser( new ExecEventParser());
		putParser( new PutEventParser());
		putParser( new RemoveEventParser());
		putParser( new ListEventParser());
		putParser( new EndEventParser());

		////////////////////////////////////////////////////////////////////////////////////////////////////////////
		

		putParser( new SchemeEventParser() {
			{
				String id = "kp";
				String name = "key-pressure";
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
				int note         = map.containsKey( ID_NOTE     ) ? SchemeUtils.toInteger(      map.get(ID_NOTE      ) ) : 63;  
				double value     = map.containsKey( ID_VALUE )    ? SchemeUtils.toDouble(       map.get(ID_VALUE     ) ) : 0d;

				outputBuffer.midiEvent(offset , port, MetroMidiMessageGen.keyPressure( ch, note, value ) ) ;

				return result;
			}
		});

		putParser( new SchemeEventParser() {
			{
				this.id = "cc";
				this.name = "control";
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
				int key          = map.containsKey( ID_KEY      ) ? SchemeUtils.toInteger(      map.get( ID_KEY      ) ) : 0;
				int value        = map.containsKey( ID_VALUE    ) ? SchemeUtils.toInteger(      map.get( ID_VALUE    ) ) : 0;

				outputBuffer.midiEvent(offset , port, MetroMidiMessageGen.controlChange( ch, key, value ));

				return result;
			}
		});

		putParser( new SchemeEventParser() {
			{
				this.id = "pc";
				this.name = "program";
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
				int value        = map.containsKey( ID_VALUE    ) ? SchemeUtils.toInteger(      map.get( ID_VALUE    ) ) : 0;

				outputBuffer.midiEvent(offset , port, MetroMidiMessageGen.programChange( ch, value ) );

				return result;
			}
		});

		putParser( new SchemeEventParser() {
			{
				this.id = "cp";
				this.name = "channel-pressure";
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
				double value     = map.containsKey( ID_VALUE )    ? SchemeUtils.toDouble(       map.get(ID_VALUE     ) ) : 0d;

				outputBuffer.midiEvent( offset , port, MetroMidiMessageGen.channelPressure( ch, value ) );

				return result;
			}
		});

		putParser( new SchemeEventParser() {
			{
				this.id = "pb";
				this.name = "pitch-bend";
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
				double value     = map.containsKey( ID_VALUE )    ? SchemeUtils.toDouble(       map.get(ID_VALUE     ) ) : 0d;

				outputBuffer.midiEvent( offset , port, MetroMidiMessageGen.pitchBend( ch, value ) );

				return result;
			}
		});

		/*
		 * Channel Mode Control Change 
		 */
		putParser( new SchemeEventParser() {
			{
				this.id = "aso";
				this.name = "all-sound-off";
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 

				outputBuffer.midiEvent( offset , port, MetroMidiMessageGen.cc_allSoundOff( ch ) );

				return result;
			}
		});

		putParser( new SchemeEventParser() {
			{
				this.id = "rac";
				this.name = "reset-all-controllers";
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 

				outputBuffer.midiEvent( offset , port, MetroMidiMessageGen.cc_resetAllControllers( ch ));

				return result;
			}
		});

		putParser( new SchemeEventParser() {
			{
				this.id = "lc";
				this.name = "local-controls";
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0;
				boolean on       = map.containsKey( ID_VALUE    ) ? SchemeUtils.toBoolean(      map.get(ID_VALUE     ) ) : false; 

				outputBuffer.midiEvent( offset , port, MetroMidiMessageGen.cc_localControls( ch, on ));

				return result;
			}
		});

		putParser( new SchemeEventParser() {
			{
				this.id = "anf";
				this.name = "all-note-off";
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 

				outputBuffer.midiEvent( offset , port, MetroMidiMessageGen.cc_allNoteOff( ch ) );

				return result;
			}
		});

		putParser( new SchemeEventParser() {
			{
				this.id = "omff";
				this.name = "omni-mode-off";
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 

				outputBuffer.midiEvent( offset , port, MetroMidiMessageGen.cc_omniModeOff( ch ) );

				return result;
			}
		});

		putParser( new SchemeEventParser() {
			{
				this.id = "omon";
				this.name = "omni-mode-on";
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 

				outputBuffer.midiEvent( offset , port, MetroMidiMessageGen.cc_omniModeOn( ch ) );

				return result;
			}
		});

		putParser( new SchemeEventParser() {
			{
				this.id = "mono";
				this.name = "mono-mode-off";
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 

				outputBuffer.midiEvent( offset , port, MetroMidiMessageGen.cc_monoModeOn( ch ) );

				return result;
			}
		});

		putParser( new SchemeEventParser() {
			{
				this.id = "poly";
				this.name = "poly-mode-on";
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 

				outputBuffer.midiEvent( offset , port, MetroMidiMessageGen.cc_polyModeOn( ch ) );

				return result;
			}
		});

		putParser( new SchemeEventParser() {
			{
				this.id = "spp";
				this.name = "song-position-pointer";
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
				int value        = map.containsKey( ID_VALUE    ) ? SchemeUtils.toInteger(      map.get( ID_VALUE    ) ) : 0;

				outputBuffer.midiEvent( offset , port, MetroMidiMessageGen.songPositionPointer( value ) );

				return result;
			}
		});

		putParser( new SchemeEventParser() {
			{
				this.id = "ss";
				this.name = "song-select";
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
				int value        = map.containsKey( ID_VALUE    ) ? SchemeUtils.toInteger(      map.get( ID_VALUE    ) ) : 0;

				outputBuffer.midiEvent( offset , port, MetroMidiMessageGen.songSelect( value ) );

				return result;
			}
		});

		putParser( new SchemeEventParser() {
			{
				this.id = "eoe";
				this.name = "end-of-exclusive";
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 

				outputBuffer.midiEvent( offset , port, MetroMidiMessageGen.endOfExclusive() );

				return result;
			}
		});

		putParser( new SchemeEventParser() {
			{
				this.id = "clock";
				this.name = "clock";
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 

				outputBuffer.midiEvent( offset , port, MetroMidiMessageGen.clock() );

				return result;
			}
		});

		putParser( new SchemeEventParser() {
			{
				this.id = "start";
				this.name = "start";
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 

				outputBuffer.midiEvent( offset , port, MetroMidiMessageGen.start() );

				return result;
			}
		});

		putParser( new SchemeEventParser() {
			{
				this.id = "cont";
				this.name = "continue";
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 

				outputBuffer.midiEvent( offset , port, MetroMidiMessageGen.cont() );

				return result;
			}
		});

		putParser( new SchemeEventParser() {
			{
				this.id = "stop";
				this.name = "stop";
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 

				outputBuffer.midiEvent( offset , port, MetroMidiMessageGen.stop() );

				return result;
			}
		});

		putParser( new SchemeEventParser() {
			{
				this.id = "reset";
				this.name = "reset";
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 

				outputBuffer.midiEvent( offset , port, MetroMidiMessageGen.reset());

				return result;
			}
		});


		/*
		 * Control Changes
		 */

		//                                   
		final int CC_BANK_SELECT                            = 0  ; 
		putParser( new SchemeMidiControlChangeEventParser() {
			{
				this.id = "bs";
				this.name = "bank-select";
				this.shortDescription = "Bank Select";
				this.description = "Allows user to switch bank for patch selection. Program change used with Bank Select. MIDI can access 16,384 patches per MIDI channel.";
				this.controlNumber = CC_BANK_SELECT                            ;
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
				int value        = map.containsKey( ID_VALUE    ) ? SchemeUtils.toInteger(      map.get( ID_VALUE    ) ) : 0;

				outputBuffer.midiEvent(offset , port, MetroMidiMessageGen.controlChange( ch, controlNumber, value ));

				return result;
			}
		});
		
		//                                    
		final int CC_MODULATION                             = 1  ; 
		putParser( new SchemeMidiControlChangeEventParser() {
			{
				this.id = "mod";
				this.name = "modulation";
				this.shortDescription = "Modulation";
				this.description = "Generally this CC controls a vibrato effect (pitch, loudness, brighness). What is modulated is based on the patch.";
				this.controlNumber = CC_MODULATION                             ;
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
				int value        = map.containsKey( ID_VALUE    ) ? SchemeUtils.toInteger(      map.get( ID_VALUE    ) ) : 0;

				outputBuffer.midiEvent(offset , port, MetroMidiMessageGen.controlChange( ch, controlNumber, value ));

				return result;
			}
		});
		
		//                             
		final int CC_BREATH_CTRL                            = 2  ; 
		putParser( new SchemeMidiControlChangeEventParser() {
			{
				this.id = "bc";
				this.name = "breath-controller";
				this.shortDescription = "Breath Controller";
				this.description = "Often times associated with aftertouch messages. It was originally intended for use with a breath MIDI controller in which blowing harder produced higher MIDI control values. It can be used for modulation as well.";
				this.controlNumber = CC_BREATH_CTRL                            ;
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
				int value        = map.containsKey( ID_VALUE    ) ? SchemeUtils.toInteger(      map.get( ID_VALUE    ) ) : 0;

				outputBuffer.midiEvent(offset , port, MetroMidiMessageGen.controlChange( ch, controlNumber, value ));

				return result;
			}
		});
		
		//                               
		final int CC_FOOT_CTRL                              = 4  ; 
		putParser( new SchemeMidiControlChangeEventParser() {
			{
				this.id = "fc";
				this.name = "foot-controller";
				this.shortDescription = "Foot Controller";
				this.description = "Often used with aftertouch messages. It can send a continuous stream of values based on how the pedal is used.";
				this.controlNumber = CC_FOOT_CTRL                              ;
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
				int value        = map.containsKey( ID_VALUE    ) ? SchemeUtils.toInteger(      map.get( ID_VALUE    ) ) : 0;

				outputBuffer.midiEvent(offset , port, MetroMidiMessageGen.controlChange( ch, controlNumber, value ));

				return result;
			}
		});
		
		//                                
		final int CC_PORTAMENTO_TIME                        = 5  ; 
		putParser( new SchemeMidiControlChangeEventParser() {
			{
				this.id = "pt";
				this.name = "portamento-time";
				this.shortDescription = "Portamento Time";
				this.description = "Controls portamento rate to slide between 2 notes played subsequently.";
				this.controlNumber = CC_PORTAMENTO_TIME                        ;
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
				int value        = map.containsKey( ID_VALUE    ) ? SchemeUtils.toInteger(      map.get( ID_VALUE    ) ) : 0;

				outputBuffer.midiEvent(offset , port, MetroMidiMessageGen.controlChange( ch, controlNumber, value ));

				return result;
			}
		});
		
		//          
		final int CC_DATA_ENTRY_MSB                         = 6  ; 
		putParser( new SchemeMidiControlChangeEventParser() {
			{
				this.id = "de-msb";
				this.name = "data-entry-msb";
				this.shortDescription = "Data Entry Most Significant Bit(MSB)";
				this.description = "Controls Value for NRPN or RPN parameters.";
				this.controlNumber = CC_DATA_ENTRY_MSB                         ;
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
				int value        = map.containsKey( ID_VALUE    ) ? SchemeUtils.toInteger(      map.get( ID_VALUE    ) ) : 0;

				outputBuffer.midiEvent(offset , port, MetroMidiMessageGen.controlChange( ch, controlNumber, value ));

				return result;
			}
		});
		
		//                                        
		final int CC_VOLUME                                 = 7  ; 
		putParser( new SchemeMidiControlChangeEventParser() {
			{
				this.id = "v";
				this.name = "volume";
				this.shortDescription = "Volume";
				this.description = "Control the volume of the channel";
				this.controlNumber = CC_VOLUME                                 ;
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
				int value        = map.containsKey( ID_VALUE    ) ? SchemeUtils.toInteger(      map.get( ID_VALUE    ) ) : 0;

				outputBuffer.midiEvent(offset , port, MetroMidiMessageGen.controlChange( ch, controlNumber, value ));

				return result;
			}
		});
		
		//                                       
		final int CC_BALANCE                                = 8  ; 
		putParser( new SchemeMidiControlChangeEventParser() {
			{
				this.id = "b";
				this.name = "balance";
				this.shortDescription = "Balance";
				this.description = "Controls the left and right balance, generally for stereo patches.0 = hard left, 64 = center, 127 = hard right";
				this.controlNumber = CC_BALANCE                                ;
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
				int value        = map.containsKey( ID_VALUE    ) ? SchemeUtils.toInteger(      map.get( ID_VALUE    ) ) : 0;

				outputBuffer.midiEvent(offset , port, MetroMidiMessageGen.controlChange( ch, controlNumber, value ));

				return result;
			}
		});
		
		//                                           
		final int CC_PAN                                    = 10 ; 
		putParser( new SchemeMidiControlChangeEventParser() {
			{
				this.id = "p";
				this.name = "pan";
				this.shortDescription = "Pan";
				this.description = "Controls the left and right balance, generally for mono patches.0 = hard left, 64 = center, 127 = hard right";
				this.controlNumber = CC_PAN                                    ;
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
				int value        = map.containsKey( ID_VALUE    ) ? SchemeUtils.toInteger(      map.get( ID_VALUE    ) ) : 0;

				outputBuffer.midiEvent(offset , port, MetroMidiMessageGen.controlChange( ch, controlNumber, value ));

				return result;
			}
		});
		
		//                                    
		final int CC_EXPRESSION                             = 11 ; 
		putParser( new SchemeMidiControlChangeEventParser() {
			{
				this.id = "e";
				this.name = "expression";
				this.shortDescription = "Expression";
				this.description = "Expression is a percentage of volume (CC7).";
				this.controlNumber = CC_EXPRESSION                             ;
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
				int value        = map.containsKey( ID_VALUE    ) ? SchemeUtils.toInteger(      map.get( ID_VALUE    ) ) : 0;

				outputBuffer.midiEvent(offset , port, MetroMidiMessageGen.controlChange( ch, controlNumber, value ));

				return result;
			}
		});
		
		//                           
		final int CC_EFFECT_CTRL_1                          = 12 ; 
		putParser( new SchemeMidiControlChangeEventParser() {
			{
				this.id = "ec1";
				this.name = "effect-controller-1";
				this.shortDescription = "Effect Controller 1";
				this.description = "Usually used to control a parameter of an effect within the synth/workstation.";
				this.controlNumber = CC_EFFECT_CTRL_1                          ;
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
				int value        = map.containsKey( ID_VALUE    ) ? SchemeUtils.toInteger(      map.get( ID_VALUE    ) ) : 0;

				outputBuffer.midiEvent(offset , port, MetroMidiMessageGen.controlChange( ch, controlNumber, value ));

				return result;
			}
		});
		
		//                           
		final int CC_EFFECT_CTRL_2                          = 13 ; 
		putParser( new SchemeMidiControlChangeEventParser() {
			{
				this.id = "ec2";
				this.name = "effect-controller-2";
				this.shortDescription = "Effect Controller 2";
				this.description = "Usually used to control a parameter of an effect within the synth/workstation.";
				this.controlNumber = CC_EFFECT_CTRL_2                          ;
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
				int value        = map.containsKey( ID_VALUE    ) ? SchemeUtils.toInteger(      map.get( ID_VALUE    ) ) : 0;

				outputBuffer.midiEvent(offset , port, MetroMidiMessageGen.controlChange( ch, controlNumber, value ));

				return result;
			}
		});
		
		//                   
		final int CC_SUSTAIN_PEDAL                          = 64 ; 
		putParser( new SchemeMidiControlChangeEventParser() {
			{
				this.id = "sp";
				this.name = "sustain-pedal";
				this.shortDescription = "Damper Pedal / Sustain Pedal";
				this.description = "On/Off switch that controls sustain. (See also Sostenuto CC 66)0 to 63 = Off, 64 to 127 = On";
				this.controlNumber = CC_SUSTAIN_PEDAL                          ;
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
				int value        = map.containsKey( ID_VALUE    ) ? SchemeUtils.toInteger(      map.get( ID_VALUE    ) ) : 0;

				outputBuffer.midiEvent(offset , port, MetroMidiMessageGen.controlChange( ch, controlNumber, value ));

				return result;
			}
		});
		
		//                      
		final int CC_PORTAMENTO_SWITCH                      = 65 ; 
		putParser( new SchemeMidiControlChangeEventParser() {
			{
				this.id = "ps";
				this.name = "portamento-switch";
				this.shortDescription = "Portamento On/Off Switch";
				this.description = "On/Off switch0 to 63 = Off, 64 to 127 = On";
				this.controlNumber = CC_PORTAMENTO_SWITCH                      ;
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
				int value        = map.containsKey( ID_VALUE    ) ? SchemeUtils.toInteger(      map.get( ID_VALUE    ) ) : 0;

				outputBuffer.midiEvent(offset , port, MetroMidiMessageGen.controlChange( ch, controlNumber, value ));

				return result;
			}
		});
		
		//                       
		final int CC_SOSTENUTO_SWITCH                       = 66 ; 
		putParser( new SchemeMidiControlChangeEventParser() {
			{
				this.id = "sos-s";
				this.name = "sostenuto-switch";
				this.shortDescription = "Sostenuto On/Off Switch";
				this.description = "On/Off switch – Like the Sustain controller (CC 64), However it only holds notes that were “On” when the pedal was pressed. People use it to “hold” chords” and play melodies over the held chord.0 to 63 = Off, 64 to 127 = On";
				this.controlNumber = CC_SOSTENUTO_SWITCH                       ;
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
				int value        = map.containsKey( ID_VALUE    ) ? SchemeUtils.toInteger(      map.get( ID_VALUE    ) ) : 0;

				outputBuffer.midiEvent(offset , port, MetroMidiMessageGen.controlChange( ch, controlNumber, value ));

				return result;
			}
		});
		
		//                      
		final int CC_SOFT_PEDAL_SWITCH                      = 67 ; 
		putParser( new SchemeMidiControlChangeEventParser() {
			{
				this.id = "soft-pedal";
				this.name = "soft-pedal-switch";
				this.shortDescription = "Soft Pedal On/Off Switch";
				this.description = "On/Off switch- Lowers the volume of notes played.0 to 63 = Off, 64 to 127 = On";
				this.controlNumber = CC_SOFT_PEDAL_SWITCH                      ;
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
				int value        = map.containsKey( ID_VALUE    ) ? SchemeUtils.toInteger(      map.get( ID_VALUE    ) ) : 0;

				outputBuffer.midiEvent(offset , port, MetroMidiMessageGen.controlChange( ch, controlNumber, value ));

				return result;
			}
		});
		
		//                             
		final int CC_LEGATO_FOOTSWITCH                      = 68 ; 
		putParser( new SchemeMidiControlChangeEventParser() {
			{
				this.id = "ls";
				this.name = "legato-switch";
				this.shortDescription = "Legato FootSwitch";
				this.description = "On/Off switch- Turns Legato effect between 2 subsequent notes On or Off.0 to 63 = Off, 64 to 127 = On";
				this.controlNumber = CC_LEGATO_FOOTSWITCH                      ;
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
				int value        = map.containsKey( ID_VALUE    ) ? SchemeUtils.toInteger(      map.get( ID_VALUE    ) ) : 0;

				outputBuffer.midiEvent(offset , port, MetroMidiMessageGen.controlChange( ch, controlNumber, value ));

				return result;
			}
		});
		
		//                                        
		final int CC_HOLD_2                                 = 69 ; 
		putParser( new SchemeMidiControlChangeEventParser() {
			{
				this.id = "h2";
				this.name = "hold-2";
				this.shortDescription = "Hold 2";
				this.description = "Another way to “hold notes” (see MIDI CC 64 and MIDI CC 66). However notes fade out according to their release parameter rather than when the pedal is released.";
				this.controlNumber = CC_HOLD_2                                 ;
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
				int value        = map.containsKey( ID_VALUE    ) ? SchemeUtils.toInteger(      map.get( ID_VALUE    ) ) : 0;

				outputBuffer.midiEvent(offset , port, MetroMidiMessageGen.controlChange( ch, controlNumber, value ));

				return result;
			}
		});
		
		//                            
		final int CC_SOUND_CTRL_01                          = 70 ; 
		putParser( new SchemeMidiControlChangeEventParser() {
			{
				this.id = "sc1";
				this.name = "sound-controller-1";
				this.shortDescription = "Sound Controller 1";
				this.description = "Usually controls the way a sound is produced. Default = Sound Variation.";
				this.controlNumber = CC_SOUND_CTRL_01                          ;
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
				int value        = map.containsKey( ID_VALUE    ) ? SchemeUtils.toInteger(      map.get( ID_VALUE    ) ) : 0;

				outputBuffer.midiEvent(offset , port, MetroMidiMessageGen.controlChange( ch, controlNumber, value ));

				return result;
			}
		});
		
		//                            
		final int CC_SOUND_CTRL_02                          = 71 ; 
		putParser( new SchemeMidiControlChangeEventParser() {
			{
				this.id = "sc2";
				this.name = "sound-controller-2";
				this.shortDescription = "Sound Controller 2";
				this.description = "Allows shaping the Voltage Controlled Filter (VCF). Default = Resonance -also(Timbre or Harmonics)";
				this.controlNumber = CC_SOUND_CTRL_02                          ;
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
				int value        = map.containsKey( ID_VALUE    ) ? SchemeUtils.toInteger(      map.get( ID_VALUE    ) ) : 0;

				outputBuffer.midiEvent(offset , port, MetroMidiMessageGen.controlChange( ch, controlNumber, value ));

				return result;
			}
		});
		
		//                            
		final int CC_SOUND_CTRL_03                          = 72 ; 
		putParser( new SchemeMidiControlChangeEventParser() {
			{
				this.id = "sc3";
				this.name = "sound-controller-3";
				this.shortDescription = "Sound Controller 3";
				this.description = "Controls release time of the Voltage controlled Amplifier (VCA). Default = Release Time.";
				this.controlNumber = CC_SOUND_CTRL_03                          ;
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
				int value        = map.containsKey( ID_VALUE    ) ? SchemeUtils.toInteger(      map.get( ID_VALUE    ) ) : 0;

				outputBuffer.midiEvent(offset , port, MetroMidiMessageGen.controlChange( ch, controlNumber, value ));

				return result;
			}
		});
		
		//                            
		final int CC_SOUND_CTRL_04                          = 73 ; 
		putParser( new SchemeMidiControlChangeEventParser() {
			{
				this.id = "sc4";
				this.name = "sound-controller-4";
				this.shortDescription = "Sound Controller 4";
				this.description = "Controls the “Attack’ of a sound. The attack is the amount of time it takes forthe sound to reach maximum amplitude.";
				this.controlNumber = CC_SOUND_CTRL_04                          ;
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
				int value        = map.containsKey( ID_VALUE    ) ? SchemeUtils.toInteger(      map.get( ID_VALUE    ) ) : 0;

				outputBuffer.midiEvent(offset , port, MetroMidiMessageGen.controlChange( ch, controlNumber, value ));

				return result;
			}
		});
		
		//                            
		final int CC_SOUND_CTRL_05                          = 74 ; 
		putParser( new SchemeMidiControlChangeEventParser() {
			{
				this.id = "sc5";
				this.name = "sound-controller-5";
				this.shortDescription = "Sound Controller 5";
				this.description = "Controls VCFs cutoff frequency of the filter.";
				this.controlNumber = CC_SOUND_CTRL_05                          ;
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
				int value        = map.containsKey( ID_VALUE    ) ? SchemeUtils.toInteger(      map.get( ID_VALUE    ) ) : 0;

				outputBuffer.midiEvent(offset , port, MetroMidiMessageGen.controlChange( ch, controlNumber, value ));

				return result;
			}
		});
		
		//                            
		final int CC_SOUND_CTRL_06                          = 75 ; 
		putParser( new SchemeMidiControlChangeEventParser() {
			{
				this.id = "sc6";
				this.name = "sound-controller-6";
				this.shortDescription = "Sound Controller 6";
				this.description = "Generic – Some manufacturers may use to further shave their sounds.";
				this.controlNumber = CC_SOUND_CTRL_06                          ;
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
				int value        = map.containsKey( ID_VALUE    ) ? SchemeUtils.toInteger(      map.get( ID_VALUE    ) ) : 0;

				outputBuffer.midiEvent(offset , port, MetroMidiMessageGen.controlChange( ch, controlNumber, value ));

				return result;
			}
		});
		
		//                            
		final int CC_SOUND_CTRL_07                          = 76 ; 
		putParser( new SchemeMidiControlChangeEventParser() {
			{
				this.id = "sc7";
				this.name = "sound-controller-7";
				this.shortDescription = "Sound Controller 7";
				this.description = "Generic – Some manufacturers may use to further shave their sounds.";
				this.controlNumber = CC_SOUND_CTRL_07                          ;
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
				int value        = map.containsKey( ID_VALUE    ) ? SchemeUtils.toInteger(      map.get( ID_VALUE    ) ) : 0;

				outputBuffer.midiEvent(offset , port, MetroMidiMessageGen.controlChange( ch, controlNumber, value ));

				return result;
			}
		});
		
		//                            
		final int CC_SOUND_CTRL_08                          = 77 ; 
		putParser( new SchemeMidiControlChangeEventParser() {
			{
				this.id = "sc8";
				this.name = "sound-controller-8";
				this.shortDescription = "Sound Controller 8";
				this.description = "Generic – Some manufacturers may use to further shave their sounds.";
				this.controlNumber = CC_SOUND_CTRL_08                          ;
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
				int value        = map.containsKey( ID_VALUE    ) ? SchemeUtils.toInteger(      map.get( ID_VALUE    ) ) : 0;

				outputBuffer.midiEvent(offset , port, MetroMidiMessageGen.controlChange( ch, controlNumber, value ));

				return result;
			}
		});
		
		//                            
		final int CC_SOUND_CTRL_09                          = 78 ; 
		putParser( new SchemeMidiControlChangeEventParser() {
			{
				this.id = "sc9";
				this.name = "sound-controller-9";
				this.shortDescription = "Sound Controller 9";
				this.description = "Generic – Some manufacturers may use to further shave their sounds.";
				this.controlNumber = CC_SOUND_CTRL_09                          ;
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
				int value        = map.containsKey( ID_VALUE    ) ? SchemeUtils.toInteger(      map.get( ID_VALUE    ) ) : 0;

				outputBuffer.midiEvent(offset , port, MetroMidiMessageGen.controlChange( ch, controlNumber, value ));

				return result;
			}
		});
		
		//                           
		final int CC_SOUND_CTRL_10                          = 79 ; 
		putParser( new SchemeMidiControlChangeEventParser() {
			{
				this.id = "sc10";
				this.name = "sound-controller-10";
				this.shortDescription = "Sound Controller 10";
				this.description = "Generic – Some manufacturers may use to further shave their sounds.";
				this.controlNumber = CC_SOUND_CTRL_10                          ;
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
				int value        = map.containsKey( ID_VALUE    ) ? SchemeUtils.toInteger(      map.get( ID_VALUE    ) ) : 0;

				outputBuffer.midiEvent(offset , port, MetroMidiMessageGen.controlChange( ch, controlNumber, value ));

				return result;
			}
		});
		
		//             
		final int CC_GENERAL_PURPOSE_01                     = 80 ; 
		putParser( new SchemeMidiControlChangeEventParser() {
			{
				this.id = "gp01";
				this.name = "general-purpose-cc-01";
				this.shortDescription = "General Purpose MIDI CC Controller";
				this.description = "GenericOn/Off switch0 to 63 = Off, 64 to 127 = On";
				this.controlNumber = CC_GENERAL_PURPOSE_01                     ;
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
				int value        = map.containsKey( ID_VALUE    ) ? SchemeUtils.toInteger(      map.get( ID_VALUE    ) ) : 0;

				outputBuffer.midiEvent(offset , port, MetroMidiMessageGen.controlChange( ch, controlNumber, value ));

				return result;
			}
		});
		
		//            
		final int CC_GENERAL_PURPOSE_02                     = 81 ; 
		putParser( new SchemeMidiControlChangeEventParser() {
			{
				this.id = "gp02";
				this.name = "general-purpose-cc-02";
				this.shortDescription = "General Purpose MIDI CC Controller";
				this.description = "GenericOn/Off switch0 to 63 = Off, 64 to 127 = On";
				this.controlNumber = CC_GENERAL_PURPOSE_02                     ;
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
				int value        = map.containsKey( ID_VALUE    ) ? SchemeUtils.toInteger(      map.get( ID_VALUE    ) ) : 0;

				outputBuffer.midiEvent(offset , port, MetroMidiMessageGen.controlChange( ch, controlNumber, value ));

				return result;
			}
		});
		
		//             
		final int CC_GENERAL_PURPOSE_03                     = 82 ; 
		putParser( new SchemeMidiControlChangeEventParser() {
			{
				this.id = "gp03";
				this.name = "general-purpose-cc-03";
				this.shortDescription = "General PurposeMIDI CC Controller";
				this.description = "GenericOn/Off switch0 to 63 = Off, 64 to 127 = On";
				this.controlNumber = CC_GENERAL_PURPOSE_03                     ;
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
				int value        = map.containsKey( ID_VALUE    ) ? SchemeUtils.toInteger(      map.get( ID_VALUE    ) ) : 0;

				outputBuffer.midiEvent(offset , port, MetroMidiMessageGen.controlChange( ch, controlNumber, value ));

				return result;
			}
		});
		
		//            
		final int CC_GENERAL_PURPOSE_04                     = 83 ; 
		putParser( new SchemeMidiControlChangeEventParser() {
			{
				this.id = "gp04";
				this.name = "general-purpose-cc-04";
				this.shortDescription = "General Purpose MIDI CC Controller";
				this.description = "GenericOn/Off switch0 to 63 = Off, 64 to 127 = On";
				this.controlNumber = CC_GENERAL_PURPOSE_04                     ;
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
				int value        = map.containsKey( ID_VALUE    ) ? SchemeUtils.toInteger(      map.get( ID_VALUE    ) ) : 0;

				outputBuffer.midiEvent(offset , port, MetroMidiMessageGen.controlChange( ch, controlNumber, value ));

				return result;
			}
		});
		
		//                         
		final int CC_PORTAMENTO_CC_CTRL                     = 84 ; 
		putParser( new SchemeMidiControlChangeEventParser() {
			{
				this.id = "po";
				this.name = "portamento";
				this.shortDescription = "Portamento CC Control";
				this.description = "Controls the amount of Portamento.";
				this.controlNumber = CC_PORTAMENTO_CC_CTRL                     ;
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
				int value        = map.containsKey( ID_VALUE    ) ? SchemeUtils.toInteger(      map.get( ID_VALUE    ) ) : 0;

				outputBuffer.midiEvent(offset , port, MetroMidiMessageGen.controlChange( ch, controlNumber, value ));

				return result;
			}
		});
		
		//                                
		final int CC_EFFECT_1_DEPTH                         = 91 ; 
		putParser( new SchemeMidiControlChangeEventParser() {
			{
				this.id = "e1";
				this.name = "effect-1";
				this.shortDescription = "Effect 1 Depth";
				this.description = "Usually controls reverb send amount";
				this.controlNumber = CC_EFFECT_1_DEPTH                         ;
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
				int value        = map.containsKey( ID_VALUE    ) ? SchemeUtils.toInteger(      map.get( ID_VALUE    ) ) : 0;

				outputBuffer.midiEvent(offset , port, MetroMidiMessageGen.controlChange( ch, controlNumber, value ));

				return result;
			}
		});
		
		//                                
		final int CC_EFFECT_2_DEPTH                         = 92 ; 
		putParser( new SchemeMidiControlChangeEventParser() {
			{
				this.id = "e2";
				this.name = "effect-2";
				this.shortDescription = "Effect 2 Depth";
				this.description = "Usually controls tremolo amount";
				this.controlNumber = CC_EFFECT_2_DEPTH                         ;
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
				int value        = map.containsKey( ID_VALUE    ) ? SchemeUtils.toInteger(      map.get( ID_VALUE    ) ) : 0;

				outputBuffer.midiEvent(offset , port, MetroMidiMessageGen.controlChange( ch, controlNumber, value ));

				return result;
			}
		});
		
		//                                
		final int CC_EFFECT_3_DEPTH                         = 93 ; 
		putParser( new SchemeMidiControlChangeEventParser() {
			{
				this.id = "e3";
				this.name = "effect-3";
				this.shortDescription = "Effect 3 Depth";
				this.description = "Usually controls chorus amount";
				this.controlNumber = CC_EFFECT_3_DEPTH                         ;
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
				int value        = map.containsKey( ID_VALUE    ) ? SchemeUtils.toInteger(      map.get( ID_VALUE    ) ) : 0;

				outputBuffer.midiEvent(offset , port, MetroMidiMessageGen.controlChange( ch, controlNumber, value ));

				return result;
			}
		});
		
		//                                
		final int CC_EFFECT_4_DEPTH                         = 94 ; 
		putParser( new SchemeMidiControlChangeEventParser() {
			{
				this.id = "e4";
				this.name = "effect-4";
				this.shortDescription = "Effect 4 Depth";
				this.description = "Usually controls detune amount";
				this.controlNumber = CC_EFFECT_4_DEPTH                         ;
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
				int value        = map.containsKey( ID_VALUE    ) ? SchemeUtils.toInteger(      map.get( ID_VALUE    ) ) : 0;

				outputBuffer.midiEvent(offset , port, MetroMidiMessageGen.controlChange( ch, controlNumber, value ));

				return result;
			}
		});
		
		//                                
		final int CC_EFFECT_5_DEPTH                         = 95 ; 
		putParser( new SchemeMidiControlChangeEventParser() {
			{
				this.id = "e5";
				this.name = "effect-5";
				this.shortDescription = "Effect 5 Depth";
				this.description = "Usually controls phaser amount";
				this.controlNumber = CC_EFFECT_5_DEPTH                         ;
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
				int value        = map.containsKey( ID_VALUE    ) ? SchemeUtils.toInteger(      map.get( ID_VALUE    ) ) : 0;

				outputBuffer.midiEvent(offset , port, MetroMidiMessageGen.controlChange( ch, controlNumber, value ));

				return result;
			}
		});
		
		//                           
		final int CC_DATA_INCREMENT                         = 96 ; 
		putParser( new SchemeMidiControlChangeEventParser() {
			{
				this.id = "inc";
				this.name = "data-increment";
				this.shortDescription = "(+1) Data Increment";
				this.description = "Usually used to increment data for RPN and NRPN messages.";
				this.controlNumber = CC_DATA_INCREMENT;
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
				int value        = map.containsKey( ID_VALUE    ) ? SchemeUtils.toInteger(      map.get( ID_VALUE    ) ) : 0;

				outputBuffer.midiEvent(offset , port, MetroMidiMessageGen.controlChange( ch, controlNumber, value ));

				return result;
			}
		});
		
		//                           
		final int CC_DATA_DECREMENT                         = 97 ;
		putParser( new SchemeMidiControlChangeEventParser() {
			{
				this.id = "dec";
				this.name = "data-decrement";
				this.shortDescription = "(-1) Data Decrement";
				this.description = "Usually used to decrement data for RPN and NRPN messages.";
				this.controlNumber = CC_DATA_DECREMENT ;
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
				int value        = map.containsKey( ID_VALUE    ) ? SchemeUtils.toInteger(      map.get( ID_VALUE    ) ) : 0;

				outputBuffer.midiEvent(offset , port, MetroMidiMessageGen.controlChange( ch, controlNumber, value ));

				return result;
			}
		});
		
		//    
		final int CC_NRPN_LSB                               = 98 ; 
		putParser( new SchemeMidiControlChangeEventParser() {
			{
				this.id = "nrpn-l";
				this.name = "nrpn-lsb";
				this.shortDescription = "Non-Registered Parameter Number LSB (NRPN)";
				this.description = "For controllers 6, 38, 96, and 97, it selects the NRPN parameter.";
				this.controlNumber = CC_NRPN_LSB                               ;
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
				int value        = map.containsKey( ID_VALUE    ) ? SchemeUtils.toInteger(      map.get( ID_VALUE    ) ) : 0;

				outputBuffer.midiEvent(offset , port, MetroMidiMessageGen.controlChange( ch, controlNumber, value ));

				return result;
			}
		});
		
		//    
		final int CC_NRPN_MSB                               = 99 ;
		putParser( new SchemeMidiControlChangeEventParser() {
			{
				this.id = "nrpn-m";
				this.name = "nrpn-msb";
				this.shortDescription = "Non-Registered Parameter Number MSB (NRPN)";
				this.description = "For controllers 6, 38, 96, and 97, it selects the NRPN parameter.";
				this.controlNumber = CC_NRPN_MSB                               ;
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
				int value        = map.containsKey( ID_VALUE    ) ? SchemeUtils.toInteger(      map.get( ID_VALUE    ) ) : 0;

				outputBuffer.midiEvent(offset , port, MetroMidiMessageGen.controlChange( ch, controlNumber, value ));

				return result;
			}
		});
		
		//         
		final int CC_RPN_LSB                                = 100; 
		putParser( new SchemeMidiControlChangeEventParser() {
			{
				this.id = "rpn-l";
				this.name = "rpn-lsb";
				this.shortDescription = "Registered Parameter Number LSB (RPN)";
				this.description = "For controllers 6, 38, 96, and 97, it selects the RPN parameter.";
				this.controlNumber = CC_RPN_LSB                                ;
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
				int value        = map.containsKey( ID_VALUE    ) ? SchemeUtils.toInteger(      map.get( ID_VALUE    ) ) : 0;

				outputBuffer.midiEvent(offset , port, MetroMidiMessageGen.controlChange( ch, controlNumber, value ));

				return result;
			}
		});
		
		//         
		final int CC_RPN_MSB                                = 101;
		putParser( new SchemeMidiControlChangeEventParser() {
			{
				this.id = "rpn-m";
				this.name = "rpn-msb";
				this.shortDescription = "Registered Parameter Number MSB (RPN)";
				this.description = "For controllers 6, 38, 96, and 97, it selects the RPN parameter.";
				this.controlNumber = CC_RPN_MSB                                ;
			}
			@Override
			boolean parseEvent(Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
				int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
				int ch           = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
				int value        = map.containsKey( ID_VALUE    ) ? SchemeUtils.toInteger(      map.get( ID_VALUE    ) ) : 0;

				outputBuffer.midiEvent(offset , port, MetroMidiMessageGen.controlChange( ch, controlNumber, value ));

				return result;
			}
		});
		

		
		// http://nickfever.com/music/midi-cc-list
		

		// 0	"Bank Select"	"Allows user to switch bank for patch selection. Program change used with Bank Select. MIDI can access 16,384 patches per MIDI channel."
		// 1	"Modulation"	"Generally this CC controls a vibrato effect (pitch, loudness, brighness). What is modulated is based on the patch."
		// 2	"Breath Controller"	"Often times associated with aftertouch messages. It was originally intended for use with a breath MIDI controller in which blowing harder produced higher MIDI control values. It can be used for modulation as well."
		// 4	"Foot Controller"	"Often used with aftertouch messages. It can send a continuous stream of values based on how the pedal is used."
		// 5	"PortamentoTime"	"Controls portamento rate to slide between 2 notes played subsequently."
		// 6	"Data Entry Most Significant Bit(MSB)"	"Controls Value for NRPN or RPN parameters."
		// 7	"Volume"	"Control the volume of the channel"
		// 8	"Balance"	"Controls the left and right balance, generally for stereo patches.0 = hard left, 64 = center, 127 = hard right"
		// 10	"Pan"	"Controls the left and right balance, generally for mono patches.0 = hard left, 64 = center, 127 = hard right"
		// 11	"Expression"	"Expression is a percentage of volume (CC7)."
		// 12	"Effect Controller 1"	"Usually used to control a parameter of an effect within the synth/workstation."
		// 13	"Effect Controller 2"	"Usually used to control a parameter of an effect within the synth/workstation."
		// 64	"Damper Pedal /Sustain Pedal"	"On/Off switch that controls sustain. (See also Sostenuto CC 66)0 to 63 = Off, 64 to 127 = On"
		// 65	"Portamento On/Off Switch"	"On/Off switch0 to 63 = Off, 64 to 127 = On"
		// 66	"Sostenuto On/Off Switch"	"On/Off switch – Like the Sustain controller (CC 64), However it only holds notes that were “On” when the pedal was pressed. People use it to “hold” chords” and play melodies over the held chord.0 to 63 = Off, 64 to 127 = On"
		// 67	"Soft Pedal On/Off Switch"	"On/Off switch- Lowers the volume of notes played.0 to 63 = Off, 64 to 127 = On"
		// 68	"Legato FootSwitch"	"On/Off switch- Turns Legato effect between 2 subsequent notes On or Off.0 to 63 = Off, 64 to 127 = On"
		// 69	"Hold 2"	"Another way to “hold notes” (see MIDI CC 64 and MIDI CC 66). However notes fade out according to their release parameter rather than when the pedal is released."
		// 70	"Sound Controller 1"	"Usually controls the way a sound is produced. Default = Sound Variation."
		// 71	"Sound Controller 2"	"Allows shaping the Voltage Controlled Filter (VCF). Default = Resonance -also(Timbre or Harmonics)"
		// 72	"Sound Controller 3"	"Controls release time of the Voltage controlled Amplifier (VCA). Default = Release Time."
		// 73	"Sound Controller 4"	"Controls the “Attack’ of a sound. The attack is the amount of time it takes forthe sound to reach maximum amplitude."
		// 74	"Sound Controller 5"	"Controls VCFs cutoff frequency of the filter."
		// 75	"Sound Controller 6"	"Generic – Some manufacturers may use to further shave their sounds."
		// 76	"Sound Controller 7"	"Generic – Some manufacturers may use to further shave their sounds."
		// 77	"Sound Controller 8"	"Generic – Some manufacturers may use to further shave their sounds."
		// 78	"Sound Controller 9"	"Generic – Some manufacturers may use to further shave their sounds."
		// 79	"Sound Controller 10"	"Generic – Some manufacturers may use to further shave their sounds."
		// 80	"General PurposeMIDI CC Controller"	"GenericOn/Off switch0 to 63 = Off, 64 to 127 = On"
		// 81	"General Purpose MIDI CC Controller"	"GenericOn/Off switch0 to 63 = Off, 64 to 127 = On"
		// 82	"General PurposeMIDI CC Controller"	"GenericOn/Off switch0 to 63 = Off, 64 to 127 = On"
		// 83	"General Purpose MIDI CC Controller"	"GenericOn/Off switch0 to 63 = Off, 64 to 127 = On"
		// 84	"Portamento CC Control"	"Controls the amount of Portamento."
		// 91	"Effect 1 Depth"	"Usually controls reverb send amount"
		// 92	"Effect 2 Depth"	"Usually controls tremolo amount"
		// 93	"Effect 3 Depth"	"Usually controls chorus amount"
		// 94	"Effect 4 Depth"	"Usually controls detune amount"
		// 95	"Effect 5 Depth"	"Usually controls phaser amount"
		// 96	"(+1) Data Increment"	"Usually used to increment data for RPN and NRPN messages."
		// 97	"(-1) Data Decrement"	"Usually used to decrement data for RPN and NRPN messages."
		// 98	"Non-Registered Parameter Number LSB (NRPN)"	"For controllers 6, 38, 96, and 97, it selects the NRPN parameter."
		// 99	"Non-Registered Parameter Number MSB (NRPN)"	"For controllers 6, 38, 96, and 97, it selects the NRPN parameter."
		// 100	"Registered Parameter Number LSB (RPN)"	"For controllers 6, 38, 96, and 97, it selects the RPN parameter."
		// 101	"Registered Parameter Number MSB (RPN)"	"For controllers 6, 38, 96, and 97, it selects the RPN parameter."

		// 3	Undefined	 
		// 9	Undefined	 
		// 14	Undefined	 
		// 15	Undefined	 
		// 16 – 19	General Purpose	 
		// 20 – 31	Undefined	 
		// 32 – 63	Controller 0-31 Least Significant Bit (LSB)	 
		// 85 – 90	Undefined	 
		// 102 – 119	Undefined	 
		// 
		// 120 to 127 are “Channel Mode Messages.”		
		// 120	All Sound Off	Mutes all sounding notes. It does so regardless of release time or sustain. (See MIDI CC 123)
		// 121	Reset All Controllers	It will reset all controllers to their default.
		// 122	Local On/Off Switch	Turns internal connection of a MIDI keyboard/workstation, etc. On or Off. If you use a computer, you will most likely want local control off to avoid notes being played twice. Once locally and twice whent the note is sent back from the computer to your keyboard.
		// 123	All Notes Off	Mutes all sounding notes. Release time will still be maintained, and notes held by sustain will not turn off until sustain pedal is depressed.
		// 124	Omni Mode Off	Sets to “Omni Off” mode.
		// 125	Omni Mode On	Sets to “Omni On” mode.
		// 126	Mono Mode	Sets device mode to Monophonic.
		// 127	Poly Mode	Sets device mode to Polyphonic.

		
		
	}
	
	public static boolean parseNote( Metro metro, Scheme scheme, MetroEventBuffer outputBuffer, boolean result, AbstractSequence list ) {
		Map<String,Object> map = SchemeUtils.list2map(list, null );
		String type      = map.containsKey( ID_TYPE ) ? SchemeUtils.symbolToString(  map.get( ID_TYPE ) ) : "";
		SchemeEventParser parser = getParser( type );
		if ( parser == null ) {
			LOGGER.log(Level.WARNING, null, "unknown type (" +  type + ")" );
			return result;
		} else {
			return parser.parseEvent( metro, scheme, outputBuffer, map, result );
		}
	}
}
