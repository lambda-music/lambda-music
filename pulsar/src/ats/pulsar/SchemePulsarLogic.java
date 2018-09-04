package ats.pulsar;

import java.util.ArrayList;
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
import gnu.lists.AbstractSequence;
import gnu.lists.Pair;
import gnu.mapping.Environment;
import gnu.mapping.Procedure;
import kawa.standard.Scheme;

public class SchemePulsarLogic extends MetroLogic.Default {
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
        Logger.getLogger(SchemePulsarLogic.class.getName()).log(Level.SEVERE, msg, e);
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
			Logger.getLogger(SchemePulsarLogic.class.getName()).log(Level.SEVERE, "", e);
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
	
		boolean result = true;
		if ( pattern != null ) {

			for ( Iterator<Object> i = pattern.iterator(); i.hasNext(); ) {
				Pair ep = (Pair)i.next();
				
				/*
				 * omitting key name is now prohibited. (Wed, 29 Aug 2018 06:26:32 +0900)
				 */
				Map<String,Object> map = SchemeUtils.list2map(ep, null );
	
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
								buf.noteHit(offset, port, channel, note, velocity, length );
								break;
							case  "on" :
								buf.noteOn(offset, port, channel, note, velocity );
								break;
							case  "off" :
								buf.noteOff(offset, port, channel, note, velocity );
								break;
						}
						break;
					}
					
					case  "bar" : {
						double length    = map.containsKey( ID_LENGTH   ) ? SchemeUtils.toDouble(       map.get(ID_LENGTH    ) ) : -1d;
						if ( length < 0 ) {
							length = 1.0d;
						}
						buf.setLength( length );
						break;
					}
					case  "exec" : {
						double offset        = map.containsKey( ID_OFFSET   )  ? SchemeUtils.toDouble(     map.get( ID_OFFSET    ) ) : 0.0d;
						Procedure procedure0 = map.containsKey( ID_PROCEDURE ) ?                (Procedure)map.get( ID_PROCEDURE ) : null;
						// See the note ... XXX_SYNC_01
						buf.exec( offset, 
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
								buf.setHumanizeOffset( min, max );
								break;
							case "velo" :
								buf.setHumanizeVelocity( min, max );
								break;
							default :
								Logger.getLogger(SchemePulsarLogic.class.getName()).log(Level.WARNING, null, "unknown key (" +  type + ")" );
								break;
						}
						break;
					}
					case  "end" : {
						result = false;
						break;
					}
					default : {
						Logger.getLogger(SchemePulsarLogic.class.getName()).log(Level.WARNING, null, "unknown type (" +  type + ")" );
					}
				}
			} // end of the loop
		}
		// buf.setLength( this.bars );
		// buf.noteShot(0, 1, 0, 73, 100 );
		return result;
	}
}
