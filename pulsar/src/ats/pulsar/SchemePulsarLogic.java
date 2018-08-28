package ats.pulsar;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import ats.metro.Metro;
import ats.metro.MetroLogic;
import ats.metro.MetroMidiEvent;
import ats.metro.MetroNoteEventBuffer;
import ats.metro.MetroNoteEventBufferSequence;
import gnu.lists.AbstractSequence;
import gnu.lists.Pair;
import gnu.mapping.Environment;
import gnu.mapping.Procedure;

public class SchemePulsarLogic extends MetroLogic.Default {
	private static final String ID_TYPE      = "type";
	private static final String ID_CHANNEL   = "chan";
	private static final String ID_PORT_NO   = "port";
	private static final String ID_PROCEDURE = "proc";
	private static final String ID_LENGTH    = "len";
	private static final String ID_VELOCITY  = "velo";
	private static final String ID_NOTE      = "note";
	private static final String ID_OFFSET    = "pos";
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

	private Environment environment;
	private Procedure procedure;
	public SchemePulsarLogic ( Environment environment, Procedure procedure ) {
		this.environment = environment;
		this.procedure = procedure;
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
			scheme2buf(metro, sequence, environment, procedure, buf);
		} catch ( RuntimeException e ) {
			Logger.getLogger(SchemePulsarLogic.class.getName()).log(Level.SEVERE, "", e);
		}
		return true;
	}

	@SuppressWarnings("unchecked")
	public static boolean scheme2buf( Metro metro, MetroNoteEventBufferSequence sequence, Environment environment, Procedure procedure, MetroNoteEventBuffer buf) {
		AbstractSequence<Object> pattern ;
		try {
			Environment.setCurrent( environment );
			pattern = (AbstractSequence<Object>) procedure.applyN( new Object[] {} );
		} catch (Throwable e) {
			e.printStackTrace();
			pattern = null;
		}

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
				Map<String,Object> map = SchemeUtils.list2map(ep,
						(Object type)->{
							switch ( SchemeUtils.symbolToString(type)) {
								case "on":
								case "off":
								case "hit":
									return (Integer idx)->{
										//	double offset, int outputPortNo, int channel, int note, int velocity 
										switch ( idx ) {
											case 0: return ID_TYPE;
											case 1: return ID_PORT_NO;
											case 2: return ID_CHANNEL;
											case 3: return ID_OFFSET;
											case 4: return ID_NOTE;
											case 5: return ID_VELOCITY;
											case 6: return ID_LENGTH;
											default : throw new RuntimeException( "cannot omit the key object." );
										}
									};
								case "bar":
									return (Integer idx)->{
										switch ( idx ) {
											case 0: return ID_TYPE;
											case 1: return ID_LENGTH;
											default : throw new RuntimeException( "cannot omit the key object." );
										}
									};
								case "exec":
									return (Integer idx)->{
										switch ( idx ) {
											case 0: return ID_TYPE;
											case 1: return ID_OFFSET;
											case 2: return ID_PROCEDURE;
											default : throw new RuntimeException( "cannot omit the key object." );
										}
									};
								case "huma":
									return (Integer idx)->{
										switch ( idx ) {
											case 0: return ID_TYPE;
											default : throw new RuntimeException( "cannot omit the key object." );
										}
									};
								case "end":
									return (Integer idx)->{
										switch ( idx ) {
											case 0: return ID_TYPE;
											default : throw new RuntimeException( "cannot omit the key object." );
										}
									};
								default : {
									throw new RuntimeException( "cannot omit the key object." );
								}
							}
						});
	
				String type      = map.containsKey( ID_TYPE     ) ? SchemeUtils.symbolToString(  map.get(ID_TYPE      ) ) : "";
	
				switch ( type ) {
					case  "hit" : {
						int port         = map.containsKey( ID_PORT_NO     ) ? SchemeUtils.toInteger(       map.get(ID_PORT_NO      ) ) : 1;
						int channel      = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(       map.get(ID_CHANNEL   ) ) : 0; 
						double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(        map.get(ID_OFFSET    ) ) : 0.0d;  
						int note         = map.containsKey( ID_NOTE     ) ? SchemeUtils.toInteger(       map.get(ID_NOTE      ) ) : 63;  
						int velocity     = map.containsKey( ID_VELOCITY ) ? SchemeUtils.toInteger(       map.get(ID_VELOCITY  ) ) : 63;
						double length    = map.containsKey( ID_LENGTH   ) ? SchemeUtils.toDouble(       map.get(ID_LENGTH    ) ) : -1d;

						buf.noteHit(offset, port, channel, note, velocity, length );
						break;
					}
					case  "on" : {
						int port         = map.containsKey( ID_PORT_NO     ) ? SchemeUtils.toInteger(       map.get(ID_PORT_NO      ) ) : 1;
						int channel      = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(       map.get(ID_CHANNEL   ) ) : 0; 
						double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(        map.get(ID_OFFSET    ) ) : 0.0d;  
						int note         = map.containsKey( ID_NOTE     ) ? SchemeUtils.toInteger(       map.get(ID_NOTE      ) ) : 63;  
						int velocity     = map.containsKey( ID_VELOCITY ) ? SchemeUtils.toInteger(       map.get(ID_VELOCITY  ) ) : 63;
//						double length    = map.containsKey( "length"   ) ? SchemeUtils.toInteger(       map.get("length"    ) ) : -1d;

						buf.noteOn(offset, port, channel, note, velocity );
						break;
					}
					case  "off" : {																														
						int port         = map.containsKey( ID_PORT_NO     ) ? SchemeUtils.toInteger(       map.get(ID_PORT_NO      ) ) : 1;
						int channel      = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(       map.get(ID_CHANNEL   ) ) : 0; 
						double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(        map.get(ID_OFFSET    ) ) : 0.0d;  
						int note         = map.containsKey( ID_NOTE     ) ? SchemeUtils.toInteger(       map.get(ID_NOTE      ) ) : 63;  
						int velocity     = map.containsKey( ID_VELOCITY ) ? SchemeUtils.toInteger(       map.get(ID_VELOCITY  ) ) : 63;
//						double length    = map.containsKey( "length"   ) ? SchemeUtils.toInteger(       map.get("length"    ) ) : -1d;
						
						buf.noteOff(offset, port, channel, note, velocity );
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
						
						buf.exec( offset, environment,  procedure0 );
						break;
					}
					case  "huma" : {
						double offset      = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get( ID_OFFSET   ) ) : 0d;
						double velocity    = map.containsKey( ID_VELOCITY ) ? SchemeUtils.toDouble(       map.get( ID_VELOCITY ) ) : 0d;
						buf.humanize(offset, velocity);
						break;
					}
					case  "end" : {
						result = false;
						break;
					}
					default : {
						Logger.getLogger(Metro.class.getName()).log(Level.WARNING, null, "unknown type (" +  type + ")" );
					}
				}
			} // end of the loop
		}
		// buf.setLength( this.bars );
		// buf.noteShot(0, 1, 0, 73, 100 );
		return result;
	}
}
