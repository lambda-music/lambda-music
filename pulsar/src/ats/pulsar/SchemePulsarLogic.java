package ats.pulsar;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import ats.metro.Metro;
import ats.metro.MetroInvokable;
import ats.metro.MetroLogic;
import ats.metro.MetroMidiEvent;
import ats.metro.MetroNoteEventBuffer;
import ats.metro.MetroNoteEventBufferSequence;
import ats.metro.MetroPlayer;
import gnu.lists.AbstractSequence;
import gnu.lists.EmptyList;
import gnu.lists.ImmutablePair;
import gnu.lists.LList;
import gnu.lists.Pair;
import gnu.mapping.Environment;
import gnu.mapping.Procedure;
import gnu.mapping.ProcedureN;
import gnu.mapping.Symbol;
import kawa.standard.Scheme;

public class SchemePulsarLogic extends MetroLogic {
	static final Logger LOGGER = Logger.getLogger(SchemePulsarLogic.class.getName());

	private static final String ID_TYPE      = "type";
	private static final String ID_ENABLED   = "enab";
	private static final String ID_CHANNEL   = "chan";
	private static final String ID_PORT_NO   = "port";
	private static final String ID_PROCEDURE = "proc";
	private static final String ID_LENGTH    = "len";
	private static final String ID_VELOCITY  = "velo";
	private static final String ID_NOTE      = "note";
	private static final String ID_OFFSET    = "pos";
	private static final String ID_KEY       = "key";
	private static final String ID_MIN       = "min";
	private static final String ID_MAX       = "max";
	private static final String ID_VALUE     = "val";
//	private static final Object ID_HUMANIZE  = "huma";
	
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
	 * Basically this class is not Scheme dependent. However only exec() method
	 * needs to refer scheme object to synchronize with the scheme execution in
	 * Pulse class. In the current policy, it is preferable to avoid referring the
	 * scheme object by wrapping invocation. But in exec() method-wise, referring
	 * to the scheme object here is not avoidable.
	 */
	final Scheme scheme;
	
	final MetroInvokable procedure;
	public SchemePulsarLogic ( Scheme scheme, MetroInvokable procedure ) {
		this.scheme = scheme;
		this.procedure = procedure;
	}
	
	static void logError( String msg, Throwable e ) {
        LOGGER.log(Level.SEVERE, msg, e);
	}
	static void logInfo( String msg ) {
//        Logger.getLogger(SchemePulsarLogic.class.getName()).log(Level.INFO, msg);
		System.err.println( msg );
	}

	@Override
	public void processInputMidiBuffer(Metro metro, List<MetroMidiEvent> in, List<MetroMidiEvent> out) {
		// out.addAll( in ); TODO ******************************
		System.err.println( "in.size()" + in.size());
		System.err.println( "out.size()" + out.size());
	}


	@Override
	public boolean processOutputNoteBuffer( Metro metro, MetroNoteEventBufferSequence sequence, MetroNoteEventBuffer buf ) {
		// System.out.println("Metro.logic.new MetroLogic() {...}.initBuffer()" );
//		buf.humanize( 0.0d, 3 );
		try {
			scheme2buf(metro, sequence, scheme, procedure, buf);
		} catch ( RuntimeException e ) {
			LOGGER.log(Level.SEVERE, "", e);
		}
		return true;
	}

	@SuppressWarnings("unchecked")
	public static boolean scheme2buf( Metro metro, MetroNoteEventBufferSequence sequence, Scheme scheme, MetroInvokable procedure, MetroNoteEventBuffer buf) {
		AbstractSequence<Object> pattern = (AbstractSequence<Object>)procedure.invoke();

		/*
		class Processor {
		}
		class NoteProcessor extends Processor {
		}
		class State {
			Processor processor = null;
		}
		State state = new State();
		*/
	
		return parse(scheme, pattern, buf, true );
	}

	public static boolean parse( Scheme scheme, AbstractSequence<Object> inputList, MetroNoteEventBuffer outputBuffer, boolean result ) {
		// boolean result = true;
		if ( inputList != null ) {
			for ( Iterator<Object> i = inputList.iterator(); i.hasNext(); ) {
				Object obj = i.next();
				if ( obj instanceof Pair ) {
					Pair record = (Pair)obj;
					result = parseNote(scheme, outputBuffer, result, record);
				} else if ( obj instanceof Boolean ) {
					continue;
				} else {
					LOGGER.log( Level.WARNING, "Unsupported object type was found. We ignored it." + obj );
				}
					
			} // end of the loop
		}
		// buf.setLength( this.bars );
		// buf.noteShot(0, 1, 0, 73, 100 );
		return result;
	}

	public static boolean parseNote(Scheme scheme, MetroNoteEventBuffer outputBuffer, 
			boolean result, AbstractSequence ep2) 
	{
		/*
		 * omitting key name is now prohibited. (Wed, 29 Aug 2018 06:26:32 +0900)
		 */
		Map<String,Object> map = SchemeUtils.list2map(ep2, null );
		String type      = map.containsKey( ID_TYPE     ) ? SchemeUtils.symbolToString(  map.get( ID_TYPE     ) ) : "";

		switch ( type ) {
			case  "void" : {
				break;
			}

			case  "hit" : 
			case  "on" :
			case  "off" :
			{
				boolean enabled      = map.containsKey( ID_ENABLED     ) ? SchemeUtils.toBoolean(       map.get(ID_ENABLED      ) ) : true;
				if ( ! enabled )
					break;

				int port         = map.containsKey( ID_PORT_NO     ) ? SchemeUtils.toInteger(       map.get(ID_PORT_NO      ) ) : 1;
				int channel      = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(       map.get(ID_CHANNEL   ) ) : 0; 
				double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(        map.get(ID_OFFSET    ) ) : 0.0d;  
				int note         = map.containsKey( ID_NOTE     ) ? SchemeUtils.toInteger(       map.get(ID_NOTE      ) ) : 63;  
				double velocity  = map.containsKey( ID_VELOCITY ) ? SchemeUtils.toDouble(       map.get(ID_VELOCITY  ) ) : 63;
				double length    = map.containsKey( ID_LENGTH   ) ? SchemeUtils.toDouble(       map.get(ID_LENGTH    ) ) : -1d;
				switch ( type ) {
					case  "hit" :
						outputBuffer.noteHit(offset, port, channel, note, velocity, length );
						break;
					case  "on" :
						outputBuffer.noteOn(offset, port, channel, note, velocity );
						break;
					case  "off" :
						outputBuffer.noteOff(offset, port, channel, note, velocity );
						break;
				}
				break;
			}

			case  "bar" : {
				double length    = map.containsKey( ID_LENGTH   ) ? SchemeUtils.toDouble(       map.get(ID_LENGTH    ) ) : -1d;
				if ( length < 0 ) {
					length = 1.0d;
				}
				outputBuffer.setLength( length );
				break;
			}
			case  "exec" : {
				double offset        = map.containsKey( ID_OFFSET   )  ? SchemeUtils.toDouble(     map.get( ID_OFFSET    ) ) : 0.0d;
				Procedure procedure0 = map.containsKey( ID_PROCEDURE ) ?                (Procedure)map.get( ID_PROCEDURE ) : null;
				// See the note ... XXX_SYNC_01
				outputBuffer.exec( offset, 
						new RunnableSchemeProcedure( 
								new InvokableSchemeProcedure(scheme /* XXX_SYNC_01 */, Environment.getCurrent(), procedure0) ) );
				break;
			}
			case  "huma" : {
				String key         = map.containsKey( ID_KEY       ) ? SchemeUtils.anyToString(    map.get( ID_KEY      ) ) : "velo";
				double min         = map.containsKey( ID_MIN       ) ? SchemeUtils.toDouble(       map.get( ID_MIN      ) ) : 0d;
				double max         = map.containsKey( ID_MAX       ) ? SchemeUtils.toDouble(       map.get( ID_MAX      ) ) : 0d;
				switch ( key ) {
					case "pos" :
						outputBuffer.setHumanizeOffset( min, max );
						break;
					case "velo" :
						outputBuffer.setHumanizeVelocity( min, max );
						break;
					default :
						LOGGER.log(Level.WARNING, null, "unknown key (" +  type + ")" );
						break;
				}
				break;
			}
			case  "list" : {
				AbstractSequence<Object> value = map.containsKey( ID_VALUE ) ? (AbstractSequence<Object> )map.get( ID_VALUE ) : null;
				if ( value != null ) {
					// *** a recursive calling ***
					result = parse(scheme, value , outputBuffer, result );
				} else {
					LOGGER.log(Level.WARNING, "Found an empty list. This might be a possible cause of problems" );
				}
				break;
			}
			case  "end" : {
				result = false;
				break;
			}
			default : {
				LOGGER.log(Level.WARNING, null, "unknown type (" +  type + ")" );
			}
		}
		return result;
	}

	LList asociationList = createPairs( this );
	public LList getAsociationList() {
		return asociationList;
	}


	public static LList createPairs( MetroPlayer player ) {
		Pair[] pairs = {
				new ImmutablePair() {
					@Override
					public Object getCar() {
						return SchemeUtils.schemeSymbol("name");
					}
					@Override
					public Object getCdr() {
						return SchemeUtils.toSchemeSymbol( player.getPlayerName() );
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
										player.getPlayerTags() , (v)->SchemeUtils.toSchemeSymbol(v) ) );
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
									player.setPlayerEnabled((Boolean)args[0]);
								}
								return player.isPlayerEnabled();
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
								player.playerRemove();
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
								return SchemeUtils.toSchemeNumber( player.getPosition() );
							};
						};
					}
				},
		};
		return LList.makeList( Arrays.asList( pairs ) );
	}
}
