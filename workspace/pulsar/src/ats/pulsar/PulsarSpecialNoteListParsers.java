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

package ats.pulsar;

import static ats.pulsar.PulsarMidiNoteListParsers.*;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.logging.Level;
import java.util.logging.Logger;

import ats.metro.Metro;
import ats.metro.MetroBufferedMidiReceiver;
import ats.metro.MetroEventBuffer;
import ats.metro.MetroTrack;
import ats.metro.MetroTrack.SyncType;
import ats.pulsar.lib.SchemeUtils;
import gnu.lists.Pair;
import gnu.mapping.Procedure;
import kawa.standard.Scheme;

/**
 * Defines special note events. 
 *  
 * @author ats
 */
public class PulsarSpecialNoteListParsers {
	static final Logger LOGGER = Logger.getLogger(PulsarSpecialNoteListParsers.class.getName());

	/*
	 * XXX this value is inconsistent now (Mon, 29 Jul 2019 09:17:14 +0900)
	 */
	public static final String SEQ_BASE = "seq-base";
	
	/**
	 * Returns a collection object which contains parser elements defined in this class. 
	 * @return
	 */
	public static Collection<NoteListParserElement> getElements() {
		return Collections.unmodifiableList( elements );
	}

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
			// RENAMED (Thu, 01 Aug 2019 13:09:08 +0900)
			this.shortName = "nop";
			this.longName = "no-operation";
		}
		@Override
		public
		boolean parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver receiver, Map<String, Object> map, boolean result) {
			return result;
		}
	}
	
	public static final NoteEventParser PARSER_NOTE = new NoteEventParser();
	static { register( PARSER_NOTE ); }
	public static class NoteEventParser extends SpecialNoteListParserElement {
		{
			this.shortName = "note";
			this.longName  = "note-on-off";
		}
		@Override
		public
		boolean parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver receiver, Map<String, Object> map, boolean result) {
			boolean enabled      = map.containsKey( PulsarMidiNoteListParsers.ID_ENABLED     ) ? SchemeUtils.toBoolean( map.get( PulsarMidiNoteListParsers.ID_ENABLED ) ) : true;
			if ( ! enabled )
				return result;

			int port         = map.containsKey( ID_PORT_NO  ) ? SchemeUtils.toInteger(      map.get(ID_PORT_NO   ) ) : 1;
			int channel      = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
			double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
			int note         = map.containsKey( ID_NOTE     ) ? SchemeUtils.toInteger(      map.get(ID_NOTE      ) ) : 63;  
			double velocity  = map.containsKey( ID_VELOCITY ) ? SchemeUtils.toDouble(       map.get(ID_VELOCITY  ) ) : 63;
			double length    = map.containsKey( ID_LENGTH   ) ? SchemeUtils.toDouble(       map.get(ID_LENGTH    ) ) : -1d;
			
			if ( length < 0 )
				length = 0.0025d;

			receiver.noteOn( offset            , port, channel, note, velocity );
			receiver.noteOff( offset + length  , port, channel, note, velocity );

			return result;
		}
	}

	public static final BarEventParser PARSER_BAR = new BarEventParser();
	static { register( PARSER_BAR ); } 
	static final class BarEventParser extends SpecialNoteListParserElement {
		{
			this.shortName = "len";
			this.longName  = "bar-length";
		}
		@Override
		public
		boolean parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver receiver, Map<String, Object> map, boolean result) {
			double value    = map.containsKey( ID_VALUE ) ? SchemeUtils.toDouble( map.get( ID_VALUE ) ) : -1.0d;
			if ( value < 0 ) {
				LOGGER.log( Level.WARNING, "a len note was found but 'val was missing. This probably a bug." );
				value = 0.0d;
			}

			// LOGGER.log( Level.INFO, "a len note = " + value );
			((MetroEventBuffer) receiver).setLength( value );
			return result;
		}
	}
	
	
	public static final ExecEventParser PARSER_EXEC = new ExecEventParser();
	static { register( PARSER_EXEC ); } 
	static final class ExecEventParser extends SpecialNoteListParserElement {
		{
			this.shortName = "exec";
			this.longName = "execute-invokable";
		}
		@Override
		public
		boolean parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver receiver, Map<String, Object> map, boolean result) {
			Pulsar pulsar = ((Pulsar)metro);

			double offset        = map.containsKey( ID_OFFSET   )  ? SchemeUtils.toDouble(     map.get( ID_OFFSET    ) ) : 0.0d;
			Procedure procedure0 = map.containsKey( ID_PROCEDURE ) ?                (Procedure)map.get( ID_PROCEDURE )   : null;
			// See the note ... XXX_SYNC_01
			((MetroEventBuffer) receiver).exec( offset, 
					pulsar.createRunnableAndInvocable( procedure0 ));
			return result;
		}
	}
//	private static final Procedure SCHEME_NOP = new ProcedureN() {
//	};
	public static final PutEventParser PARSER_PUT = new PutEventParser();
	static { register( PARSER_PUT ); }
	
	static int tempNewIdCounter = 0;
	synchronized static String createTempNewId() {
		return "TEMPID-" + ( tempNewIdCounter++ );
	}
	static final class PutEventParser extends SpecialNoteListParserElement {

		{
			// RENAMED (Thu, 01 Aug 2019 13:09:08 +0900)
			this.shortName = "add";
			this.longName  = "add-track";
//			this.longName = "add a new track";
		}
		@Override
		public
		boolean parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver receiver, Map<String, Object> map, boolean result) {
//			Scheme scheme2 = ((Pulsar)metro).getScheme();
			Pulsar pulsar = ((Pulsar)metro);
			
			List<String> el = Collections.emptyList();

			double offset            = getValue( map, ID_OFFSET, 0.0d, (v)-> SchemeUtils.toDouble( v )   );
			String id                = getValue( map, "id",   null, (v)-> SchemeUtils.anyToString(    SchemeUtils.schemeNullCheck( v ) ) );
			Collection<String> tags  = getValue( map, "tags", el,   (v)-> SchemeUtils.symbolListToStringList((Pair)v ) );
			SyncType syncType        = getValue( map, "syty", SyncType.SERIAL, (v)-> SyncType.valueOf( SchemeUtils.symbolToString( SchemeUtils.schemeNullCheck( v ) ).toUpperCase() ) );
			String syncSequenceId    = getValue( map, "syid", SEQ_BASE, (v)-> SchemeUtils.anyToString(    SchemeUtils.schemeNullCheck( v ) ) );
			double syncOffset        = getValue( map, "syof", 0.0d, (v)-> SchemeUtils.toDouble( v )   );
			Procedure procedure      = getValue( map, ID_PROCEDURE, null, (v)->(Procedure)v );

			if ( id == null ) {
				// MODIFIED (Tue, 23 Jul 2019 05:55:01 +0900)
				// id = createDefaultId( pulsar.getScheme() );
				id = createTempNewId();
			}

			if ( procedure != null ) {
				String id2= id;
				// See the note ... XXX_SYNC_01
				((MetroEventBuffer) receiver).exec( offset, new Runnable() {
					@Override
					public void run() {
						SchemeSequence sequence = new SchemeSequence( pulsar.createInvokable(procedure) );
						
						MetroTrack syncTrack;
						// synchronized block added at (Mon, 29 Jul 2019 13:36:52 +0900)
						synchronized ( metro.getMetroLock() ) {
							if ( syncSequenceId == null ) {
								syncTrack = null;
							} else {
								syncTrack = pulsar.searchTrack( syncSequenceId );
								if ( syncTrack == null ) {
									logWarn( "PARSER_PUT : syncTrackId '" + syncSequenceId + "' was not found and it was ignored. " );
								}
							}
							pulsar.putTrack( 
								pulsar.createTrack( id2, tags, sequence, syncType, syncTrack, syncOffset ) );
						}
					}
				});
			}
			return result;
		}
		
		/*
		 * DEPRECATED? (Tue, 23 Jul 2019 05:57:34 +0900)
		 */
		@Deprecated
		public String createDefaultId(Scheme scheme) {
			synchronized ( scheme ) {
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
	}
	public static final RemoveEventParser PARSER_REMOVE = new RemoveEventParser();
	static { register( PARSER_REMOVE ); }
	static final class RemoveEventParser extends SpecialNoteListParserElement {
		{
			// RENAMED (Thu, 01 Aug 2019 13:09:08 +0900)
			this.shortName = "del";
			this.longName  = "del-track";
//			this.longName = "Remove the specified track";
		}
		@Override
		public
		boolean parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver receiver, Map<String, Object> map, boolean result) {
			List<String> el = Collections.emptyList();

			double offset            = getValue( map, ID_OFFSET, 0.0d, (v)-> SchemeUtils.toDouble( v )   );
			String id                = getValue( map, "id",   null, (v)-> SchemeUtils.anyToString(    SchemeUtils.schemeNullCheck( v ) ) );
			Collection<String> tags  = getValue( map, "tags", el,   (v)-> SchemeUtils.symbolListToStringList((Pair)v ) );
			
			Pulsar pulsar = (Pulsar)metro;

			if ( id != null ) {
				((MetroEventBuffer) receiver).exec( offset, new Runnable() {
					@Override
					public void run() {
						// synchronized block added at (Mon, 29 Jul 2019 13:36:52 +0900)
						synchronized ( pulsar.getMetroLock() ) {
							MetroTrack t = pulsar.searchTrack( id );
							if ( t != null ) {
								t.removeTrack(true, XXX, xxxull);
							} else {
								logWarn( "PARSER_REMOVE : id '"+ id + "' was not found." );
							}
							
// 							>>> DELETED (Thu, 01 Aug 2019 13:09:08 +0900)
//							if ( t != null ) {
//								pulsar.removeTrack( t );
//							} else {
//								logWarn( "PARSER_REMOVE : id '"+ id + "' was not found." );
//							}
//							<<< DELETED (Thu, 01 Aug 2019 13:09:08 +0900)
						}
					}
				} );
			}

			if ( tags != null ) {
				((MetroEventBuffer) receiver).exec( offset, new Runnable() {
					@Override
					public void run() {
						// synchronized block added at (Mon, 29 Jul 2019 13:36:52 +0900)
						synchronized ( pulsar.getMetroLock() ) {
							for ( MetroTrack t : pulsar.getTrackByTagSet(tags) ) {
								t.removeTrack(true, XXX, xxxull);
							}
// 							>>> DELETED (Thu, 01 Aug 2019 13:09:08 +0900)
//							pulsar.removeAllTracks( pulsar.getTrackByTagSet(tags) );
//							<<< DELETED (Thu, 01 Aug 2019 13:09:08 +0900)
						}
					}
				} );
			}
			return result;
		}
	}
	
	public static final EndEventParser PARSER_END = new EndEventParser();
	static { register( PARSER_END ); }
	static final class EndEventParser extends SpecialNoteListParserElement {
		{
			this.shortName = "end";
			this.longName = "end-of-track";
		}
		@Override
		public
		boolean parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver receiver, Map<String, Object> map, boolean result) {
			return false;
		}
	}

}
