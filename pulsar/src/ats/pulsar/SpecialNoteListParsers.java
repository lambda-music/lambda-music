package ats.pulsar;

import static ats.pulsar.MidiNoteListParsers.*;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.logging.Level;
import java.util.logging.Logger;

import ats.metro.Metro;
import ats.metro.MetroMidiDef;
import ats.metro.MetroNoteEventBuffer;
import ats.metro.MetroNoteEventBufferSequence.SyncType;
import ats.pulsar.MidiNoteListParsers.MidiNoteListParserElement;
import ats.pulsar.lib.SchemeUtils;
import gnu.lists.AbstractSequence;
import gnu.lists.Pair;
import gnu.mapping.Environment;
import gnu.mapping.Procedure;
import kawa.standard.Scheme;

/**
 * Defines special note events. 
 *  
 * @author ats
 */
public class SpecialNoteListParsers {
	static final Logger LOGGER = Logger.getLogger(SpecialNoteListParsers.class.getName());

	/**
	 * Returns a collection object which contains parser elements defined in this class. 
	 * @return
	 */
	public static Collection<NoteListParserElement> getElements() {
		return Collections.unmodifiableList( elements );
	}


//	static final String ID_CHANNEL   = "chan";
//	static final String ID_PORT_NO   = "port";
//	static final String ID_PROCEDURE = "proc";
//	static final String ID_ID        = "id";
//	static final String ID_LENGTH    = "len";
//	static final String ID_VELOCITY  = "velo";
//	static final String ID_NOTE      = "note";
//	static final String ID_OFFSET    = "pos";
//	static final String ID_KEY       = "key";
//	static final String ID_MIN       = "min";
//	static final String ID_MAX       = "max";
//	static final String ID_VALUE     = "val";

	static void logError(String msg, Throwable e) {
		LOGGER.log(Level.SEVERE, msg, e);
	}

	static void logInfo(String msg) {
		// LOGGER.log(Level.INFO, msg);
		System.err.println(msg);
	}

	static void logWarn(String msg) {
		LOGGER.log(Level.WARNING, msg);
	}

	static final <V> V getValue( Map<String,Object> map, String key, V defaultValue, Function<Object,V> converter ) {
		if ( map.containsKey(key) )
			return converter.apply( map.get(key) );
		else
			return defaultValue;
	}
	
	static ArrayList<SpecialNoteListParserElement> elements = new ArrayList<>();
	static void register( SpecialNoteListParserElement info ) {
		elements.add( info );
	}
	
	static abstract class SpecialNoteListParserElement extends NoteListParserElement {
		String shortName;
		String longName;
		@Override
		public String getShortName() {
			return shortName;
		}
		@Override
		public String getLongName() {
			return longName;
		}
	}

	public static final VoidEventParser PARSER_VOID = new VoidEventParser();
	static { register( PARSER_VOID ); }
	public static final class VoidEventParser extends SpecialNoteListParserElement {
		{
			this.shortName = "void";
			this.longName = "void";
		}
		@Override
		public
		boolean parseEvent(Metro metro, Scheme scheme, MetroNoteEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
			return result;
		}
	}
	
	public static final NoteEventParser PARSER_NOTE = new NoteEventParser();
	static { register( PARSER_NOTE ); }
	public static class NoteEventParser extends SpecialNoteListParserElement {
		@Override
		public
		boolean parseEvent(Metro metro, Scheme scheme, MetroNoteEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
			boolean enabled      = map.containsKey( MidiNoteListParsers.ID_ENABLED     ) ? SchemeUtils.toBoolean( map.get( MidiNoteListParsers.ID_ENABLED ) ) : true;
			if ( ! enabled )
				return result;

			int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
			int channel      = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
			double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
			int note         = map.containsKey( ID_NOTE     ) ? SchemeUtils.toInteger(      map.get(ID_NOTE      ) ) : 63;  
			double velocity  = map.containsKey( ID_VELOCITY ) ? SchemeUtils.toDouble(       map.get(ID_VELOCITY  ) ) : 63;
			double length    = map.containsKey( ID_LENGTH   ) ? SchemeUtils.toDouble(       map.get(ID_LENGTH    ) ) : -1d;
			
//			duration = 0.5;
			if ( length < 0 )
				length = 0.0025d;

			MidiNoteListParsers.PARSER_NOTE_ON.parseEvent(metro, scheme, buf, map, result)
			outputBuffer.midiEvent(offset             , port, MetroMidiDef.noteOn (channel, note, velocity ) );
			outputBuffer.midiEvent(offset + length  , port, MetroMidiDef.noteOff(channel, note, velocity ) );

			return result;
		}
	}

	static final class BarEventParser extends SpecialNoteListParserElement {
		{
			this.shortName = "len";
			this.longName = "bar length";
		}
		@Override
		public
		boolean parseEvent(Metro metro, Scheme scheme, MetroNoteEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
			double value    = map.containsKey( ID_VALUE ) ? SchemeUtils.toDouble( map.get( ID_VALUE ) ) : -1.0d;
			if ( value < 0 ) {
				LOGGER.log( Level.WARNING, "a len note was found but 'val was missing. This probably a bug." );
				value = 0.0d;
			}

			outputBuffer.setLength( value );
			return result;
		}
	}
	static final class ExecEventParser extends SpecialNoteListParserElement {
		{
			this.shortName = "exec";
			this.longName = "execute procedure";
		}
		@Override
		public
		boolean parseEvent(Metro metro, Scheme scheme, MetroNoteEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
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
	static final class PutEventParser extends SpecialNoteListParserElement {
		{
			this.shortName = "add";
			this.longName = "add a new sequence";
		}
		@Override
		public
		boolean parseEvent(Metro metro, Scheme scheme, MetroNoteEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
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
						SchemePulsarLogic logic = new SchemePulsarLogic( scheme,
								new InvocableSchemeProcedure( scheme, Environment.getCurrent(), procedure ) );

						pulsar.putLogic( id2, tags, logic, syncType, syncSequenceId, syncOffset  );
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
	static final class RemoveEventParser extends SpecialNoteListParserElement {
		{
			this.shortName = "kill";
			this.longName = "Remove the specified sequence";
		}
		@Override
		public
		boolean parseEvent(Metro metro, Scheme scheme, MetroNoteEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
			List<String> el = Collections.emptyList();

			double offset            = getValue( map, ID_OFFSET, 0.0d, (v)-> SchemeUtils.toDouble( v )   );
			String id                = getValue( map, "id",   null, (v)-> SchemeUtils.anyToString(    SchemeUtils.schemeNullCheck( v ) ) );
			Collection<String> tags  = getValue( map, "tags", el,   (v)-> SchemeUtils.symbolListToStringList((Pair)v ) );
			
			Pulsar pulsar = (Pulsar)metro;


			if ( id != null ) {
				outputBuffer.exec( offset, new Runnable() {
					@Override
					public void run() {
						pulsar.removeSequence( id );
					}
				} );
			}

			if ( tags != null ) {
				outputBuffer.exec( offset, new Runnable() {
					@Override
					public void run() {
						pulsar.removeSequenceAll( pulsar.getSequenceByTags(tags) );
					}
				} );
			}
			return result;
		}
	}
	
	static final class ListEventParser extends SpecialNoteListParserElement {
		{
			this.shortName = "list";
			this.longName = "list";
		}
		@Override
		public
		boolean parseEvent(Metro metro, Scheme scheme, MetroNoteEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
			AbstractSequence<Object> value = map.containsKey( ID_VALUE ) ? (AbstractSequence<Object> )map.get( ID_VALUE ) : null;
			if ( value != null ) {
				// *** a recursive calling ***
				result = SchemeNoteParser0.parse(metro, scheme , value, outputBuffer, result );
			} else {
				LOGGER.log(Level.WARNING, "Found an empty list. This might be a possible cause of problems" );
			}
			return result;
		}
	}
	static final class EndEventParser extends SpecialNoteListParserElement {
		{
			this.shortName = "end";
			this.longName = "end the sequence";
		}
		@Override
		public
		boolean parseEvent(Metro metro, Scheme scheme, MetroNoteEventBuffer outputBuffer, Map<String, Object> map, boolean result) {
			return false;
		}
	}

}
