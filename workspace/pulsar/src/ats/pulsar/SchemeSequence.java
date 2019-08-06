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

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import ats.metro.Metro;
import ats.metro.MetroAbstractMidiEvent;
import ats.metro.MetroEventBuffer;
import ats.metro.MetroSequence;
import ats.metro.MetroTrack;
import ats.metro.MetroTrackInfo;
import ats.pulsar.lib.SchemeUtils;
import ats.pulsar.lib.secretary.Invokable;
import gnu.lists.AbstractSequence;
import gnu.lists.ImmutablePair;
import gnu.lists.LList;
import gnu.lists.Pair;
import gnu.mapping.ProcedureN;
import gnu.mapping.Symbol;

public class SchemeSequence extends MetroSequence {
	static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
	static void logError(String msg, Throwable e) {
		LOGGER.log(Level.SEVERE, msg, e);
	}
	static void logInfo(String msg) {
		LOGGER.log(Level.INFO, msg);
	}
	static void logWarn(String msg) {
		LOGGER.log(Level.WARNING, msg);
	}
	
	/*
	 * (list
	 *     (cons 1.0  (lambda() ... ))
	 *     (cons 2.0  (lambda() ... ))
	 *     (cons 2.5  (lambda() ... )))
	 */
	static ArrayList<Pair> parseListOfPairs( Pair pair ) {
		ArrayList<Pair> pairs = new ArrayList<Pair>();
		for ( Object pp : pair ) {
			pairs.add( (Pair)pp ); 
		}
		return pairs;
	}

	/*
	 * Note (XXX_SYNC_01):
	 * It is not necessary to refer to the Kawa scheme instance to invoke the invokable.
	 * It is necessary because 
	 * 
	 */
	final Invokable procedure;
	public SchemeSequence ( Invokable procedure ) {
		this.procedure = procedure;
	}

	@Override
	public void processDirect(Metro metro, List<MetroAbstractMidiEvent> in, List<MetroAbstractMidiEvent> out) {
		// out.addAll( in ); TODO ******************************
		System.err.println( "in.size()" + in.size());
		System.err.println( "out.size()" + out.size());
	}


	@Override
	public boolean processBuffered( Metro metro, MetroTrack track, MetroEventBuffer buf ) {
		// System.out.println("Metro.sequence.new MetroSequence() {...}.initBuffer()" );
//		buf.humanize( 0.0d, 3 );
		boolean result = false;
		try {
			result = scheme2buf(metro, track, procedure, buf);
		} catch ( RuntimeException e ) {
			System.err.println(e );
			LOGGER.log(Level.SEVERE, "", e);
		}
		return result;
	}
	private static final NoteListParser PARSER = PulsarNoteListParser.getInstance();
	public static boolean scheme2buf( Metro metro, MetroTrack track, Invokable procedure, MetroEventBuffer buf) {
		// Call the invokable to get a note list of the next measure.
		AbstractSequence<Object> pattern = (AbstractSequence<Object>)procedure.invoke();
		
		// Parse the retrieved list to execute.
		
//		return SchemeNoteParser0.parse(metro, scheme, pattern, buf, true );
//		return SchemeNoteParser1.parse(metro, scheme, pattern, buf, true );
//		return PulsarNoteParser2.parse(metro, track, pattern, buf, true );
		return PARSER.parse( metro, track, pattern, buf, true );
	}

	LList asociationList = createPairs( this );
	public LList getAsociationList() {
		return asociationList;
	}


	@Deprecated
	public static LList createPairs( MetroTrackInfo player ) {
		Pair[] pairs = {
				new ImmutablePair() {
					@Override
					public Object getCar() {
						return SchemeUtils.schemeSymbol("name");
					}
					@Override
					public Object getCdr() {
						return player.getTrackName();
					}
				},
				new ImmutablePair() {
					@Override
					public Object getCar() {
						return SchemeUtils.schemeSymbol("tags");
					}
					@Override
					public Object getCdr() {
						return LList.makeList((List)
								SchemeUtils.<String,Symbol>convertList(
										new ArrayList( player.getTrackTags() ) , (v)->SchemeUtils.toSchemeSymbol(v) ) );
					}
				},
				new ImmutablePair() {
					@Override
					public Object getCar() {
						return SchemeUtils.schemeSymbol("playing");
					}
					@Override
					public Object getCdr() {
						return new ProcedureN() {
							public Object applyN(Object[] args) throws Throwable {
								if ( 0 < args.length ) {
									player.setTrackEnabled((Boolean)args[0]);
								}
								return player.isTrackEnabled();
							};
						};
					}
				},
				new ImmutablePair() {
					@Override
					public Object getCar() {
						return SchemeUtils.schemeSymbol( "remove" );
					}
					@Override
					public Object getCdr() {
						return new ProcedureN() {
							public Object applyN(Object[] args) throws Throwable {
								// XXX
								throw new UnsupportedOperationException();
							};
						};
					}
				},
				new ImmutablePair() {
					@Override
					public Object getCar() {
						return SchemeUtils.schemeSymbol( "position" );
					}
					@Override
					public Object getCdr() {
						return new ProcedureN() {
							public Object applyN(Object[] args) throws Throwable {
								return SchemeUtils.toSchemeNumber( player.getTrackPosition() );
							};
						};
					}
				},
		};
		return LList.makeList( Arrays.asList( pairs ) );
	}
}
