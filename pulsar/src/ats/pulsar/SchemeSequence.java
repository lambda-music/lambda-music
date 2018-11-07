package ats.pulsar;

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
import gnu.lists.AbstractSequence;
import gnu.lists.EmptyList;
import gnu.lists.ImmutablePair;
import gnu.lists.LList;
import gnu.lists.Pair;
import gnu.mapping.ProcedureN;
import gnu.mapping.Symbol;
import kawa.standard.Scheme;

public class SchemeSequence extends MetroSequence {
	static final Logger LOGGER = Logger.getLogger(SchemeSequence.class.getName());
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
	 * It is not necessary to refer to the Kawa scheme instance to invoke the procedure.
	 * It is necessary because 
	 * 
	 */
	final Scheme scheme;
	
	final Invocable procedure;
	public SchemeSequence ( Scheme scheme, Invocable procedure ) {
		this.scheme = scheme;
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

	public static boolean scheme2buf( Metro metro, MetroTrack track, Invocable procedure, MetroEventBuffer buf) {
		// Call the procedure to get a note list of the next measure.
		AbstractSequence<Object> pattern = (AbstractSequence<Object>)procedure.invoke();
		
		// Parse the retrieved list to execute.
		
//		return SchemeNoteParser0.parse(metro, scheme, pattern, buf, true );
//		return SchemeNoteParser1.parse(metro, scheme, pattern, buf, true );
		return PulsarNoteParser.parse(metro, track, pattern, buf, true );
	}

	LList asociationList = createPairs( this );
	public LList getAsociationList() {
		return asociationList;
	}


	public static LList createPairs( MetroTrackInfo player ) {
		Pair[] pairs = {
				new ImmutablePair() {
					@Override
					public Object getCar() {
						return SchemeUtils.schemeSymbol("name");
					}
					@Override
					public Object getCdr() {
						return SchemeUtils.toSchemeSymbol( player.getTrackName() );
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
										player.getTrackTags() , (v)->SchemeUtils.toSchemeSymbol(v) ) );
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
								player.removeTrack( SchemeUtils.toBoolean( args[0]) );
								return EmptyList.emptyList;
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
